
%%% _________________________________________________________________________
%%%
%%% Author:           rikjoh@gmail.com
%%% _________________________________________________________________________
%%%
%%%
%%%   TODO: <Description of module>
%%%   
%%% _________________________________________________________________________


-module(ecursed).

-revision('$Revision: $ ').
-modified('$Date: $ ').

-include("ecursed.hrl").

-define(dbg(F,A), dbg(?MODULE,?LINE,F,A)).

%% -export([]).

-compile(export_all).

-record(screen, {hx, wy, nxy, windows, selected, sel_pid, next_wid=0, clipboard}).


-record(window, {id,          % window id
		 scx,         % window upper left screen offset rows
		 scy,         % window upper left screen offset cols
		 x,           % window size rows
		 y,           % window size cols
		 maxy,        % window max size y
		 cx,          % current cursor location x
		 cy,          % current cursor location y
		 need_update, % window needs refresh, i.e. clear before draw
		 show,        % should the window be drawn on refresh
		 z,           % depth, 0 is frontmost, higher are behind
		 scroll,      % bool(), should content scroll when full
		 wrap,        % bool(), wrap lines
		 attrs,       % current sgr attrs
		 header,      % header text
		 footer,      % footer text
		 hxd,         % header n rows
		 fxd,         % footer n rows
		 cpid,        % controlling pid
		 view,        % window content
		 buff,        % integer(), buffer n lines,
		 buff_scoff   % buffer scroll offset, view n lines back from "history"
}).

-record(form, {form, form_cb, focus_field}).
-record(form_field, {num, key, slogan, type, value, field}).
-record(read_str, {window, active, x, y, prompt, head, tail, filter, suspend_on}). 
%% window = window() | undefined, undefined => screen coordinates
-record(select, {window, active, x, y, prompt, choices, curr_sel, vpos, suspend_on}).


-record(macsess, {scrnctl, outctl, consolectl, inctl}).
 
dbg(M, L,F,A) ->
    case whereis(logctl) of
	undefined ->
	    ok;
	_ ->
	    S = lists:flatten(io_lib:format("[~p:~p] "++F++"\n", [M, L | A])),
	    logctl ! {msg, S}
    end.


cmsg(M,L,Lvl,F,A) ->
    case consolectl() of
	undefined ->
	    ok;
	ConsoleCtl ->
	    Loc = lists:flatten(io_lib:format("[~p:~p] ", [M, L])),
	    
	    S = lists:flatten(io_lib:format(F++"\n", A)),
	    ConsoleCtl ! {msg, Lvl, Loc, S}
    end.




init(Args) ->
    Session=init_ctl(Args),

    receive after 100 -> sc_csi("2J") end,
    init_wpid(Session),

    Session.



init_ctl() ->
    init_ctl([]).

init_ctl(_Args) ->
    OutCtl = 
	spawn_link(fun() ->
			   outloop()
		   end),
    
    ScrnCtl = 
	spawn_link(fun() ->
			   {ok,X} = io:rows(),
			   {ok,Y} = io:columns(),
			   
			   Wins = ets:new(windows, [{keypos, #window.id},public]),
			   
			   scrnctl_loop_init(#screen{hx=X, wy=Y, windows=Wins})
		   end),
    
    
    ConsoleCtl = 
	spawn(fun() ->
		      consoleloop_init()
	      end),
    
    
    InCtl = 
	spawn_link(fun() ->
			   InLoop = self(),
			   spawn_link(fun() ->
					      P = erlang:open_port({fd,0,1},[stream]),
					      drv_loop(P, InLoop)
				      end),
			   %% 
			   
			   sc_csi("?25l"),
			   inloop_init(),
			   sc_csi("?25h")
		   end),
    
    Session =  #macsess{scrnctl=ScrnCtl, outctl=OutCtl, consolectl=ConsoleCtl, inctl=InCtl},
    
    %% OutCtl ! Session,
    ScrnCtl ! Session,
    ConsoleCtl ! Session,
    InCtl ! Session,

    Session.
    
     


init_wpid(#macsess{} = Session) ->
    put(macsess, Session),
    ok.



%% wait_inctl     
%% wait for inctl to finish
    
wait_inctl(#macsess{inctl=InCtl} = _Session) ->    
    wait_inctl(InCtl);

wait_inctl(InCtl) ->
    MRef = erlang:monitor(process, InCtl),
    receive 
	{'DOWN', MRef, _, _, _}->
	    ok
    end.


%% Client API

%% newwin
%%   X  : height
%%   Y  : width
%%   OX : Screen X (rows) offset
%%   OY : Screen Y (cols) offset
newwin(X,Y,OX,OY) ->
    newwin(X,Y,OX,OY, []).
newwin(X,Y,OX,OY, WAttrs) ->
    scrnctl({newwin,X,Y,OX,OY,WAttrs}).

%% wheader
%% Add + set header for window
wheader(W,Hdr) ->
    scrnctl({wheader,W,Hdr}).

wfooter(W,Ftr) ->
    scrnctl({wfooter,W,Ftr}).

wclear(W) ->
    scrnctl({wclear, W}).

whide(W) ->
    scrnctl({whide, W}).

wshow(W) ->
    scrnctl({wshow,W}).

%% wmvaddstr
%% Write FA at location X,Y in window W
%% requires refresh

wmv(W,X,Y) ->
    scrnctl({wmv,W,X,Y}).

wmvaddstr(W,X,Y,F,A) ->
    Str = lists:flatten(io_lib:format(F,A)),
    wmvaddstr(W,X,Y,Str).

wmvaddstr(W,X,Y,Str) ->
    Op = sc_op(wmvaddstr,[W,X,Y,Str]),
    scrnctl(Op).

wmvaddattrstr(W,X,Y,Attrs,F,A) ->
    Str = lists:flatten(io_lib:format(F,A)),
    wmvaddattrstr(W,X,Y,Attrs,Str).

wmvaddattrstr(W,X,Y,Attrs,Str) ->
    scrnctl({wmvaddattrstr,W,X,Y,Attrs,Str}).

waddstr(W,F,A) ->
    Str = lists:flatten(io_lib:format(F,A)),
    waddstr(W,Str).

waddstr(W,S) ->
    scrnctl({waddstr,W,S}).

waddattrstr(W,Attrs,F,A) ->
    Str = lists:flatten(io_lib:format(F,A)),
    waddattrstr(W,Attrs,Str).

waddattrstr(W,Attrs,S) ->
    scrnctl({waddattrstr,W,Attrs,S}).

wsetattr(W,Attr) ->
    scrnctl({wsetattr,W,Attr}).

wscrollup(W) ->
    wscrollup(W,1).
wscrollup(W,N) ->
    scrnctl({wscrollup,W,N}).
    
wscrolldown(W) ->
    wscrolldown(W,1).
wscrolldown(W,N) ->
    scrnctl({wscrolldown,W,N}).
    
wscrollbottom(W) ->
    scrnctl({wscrollbottom,W}).
    

wresize(W,NX,NY) ->
    scrnctl({wresize,W,NX,NY}).    

wrefresh(W) ->
    scrnctl({wrefresh,W}).

screfresh() ->
    scrnctl(screfresh).
    
wselect(W) ->
    scrnctl({wselect,W}).

wget_selected() ->
    scrnctl(get_selected).

wget_xy(W) ->
    scrnctl({wget_xy, W}).

wget_cxy(W) ->
    scrnctl({wget_cxy, W}).

w2scr_xy(W,WX,WY) ->
    scrnctl({w2scr_xy,W,WX,WY}).

wpid(W, Pid) ->
    scrnctl({wpid, W, Pid}).

wargs(W, Args) ->
    scrnctl({wargs, W, Args}).

wkilleol(W) ->
    scrnctl({wkilleol, W}).

wbindconsole(W) ->
    consolectl({window, W}),
    wpid(W, consolectl()).

to_clipboard(Str) ->
    scrnctl({to_clipboard, Str}).

from_clipboard() ->
    scrnctl(from_clipboard).



sc_op(wmvaddstr, [W,X,Y,Str]) ->
    {wmvaddstr,W,X,Y,Str};
	
sc_op(waddstr, [W,S]) ->
    {waddstr,W,S};

sc_op(waddattrstr, [W,Attrs,S]) ->
    {waddattrstr,W,Attrs,S};

sc_op(wsetattr, [W,Attr]) ->
    {wsetattr,W,Attr};

sc_op(wresize, [W,NX,NY]) ->
    {wresize,W,NX,NY};    

sc_op(wrefresh, [W]) ->
    {wrefresh,W};

sc_op(screfresh, []) ->
    screfresh;
    
sc_op(wselect, [W]) ->
    {wselect,W};

sc_op(wpid, [W, Pid]) ->
    {wpid, W, Pid};

sc_op(wargs, [W, Args]) ->
    {wargs, W, Args}.




%% END API
%% --------------------------------------------------------


%% Internals
	
scrnctl() ->
    case get(macsess) of
	#macsess{scrnctl=ScrnCtl} ->
	    ScrnCtl;
	_ ->
	    undefined
    end.

scrnctl(Req) ->
    ?dbg("Req ~p", [Req]),
    case scrnctl() of 
	ScrnCtl when is_pid(ScrnCtl)  ->
	    Ref = make_ref(),
	    ScrnCtl ! {cmd, self(), Ref, Req},
	    receive 
		{Ref, Resp} ->
		    Resp
	    after 2000 ->
		    {error, timeout}
	    end;
	_ ->
	    {error, bad_session}
    end.

ascrnctl(Req) ->
    case scrnctl() of
	ScrnCtl when is_pid(ScrnCtl)  ->
	    ScrnCtl ! {acmd, Req};
	_ ->
	    {error, bad_session}
    end.


consolectl() ->
    case get(macsess) of
	#macsess{consolectl=ConsoleCtl} ->
	    ConsoleCtl;
	_ ->
	    undefined
    end.

consolectl(Msg) ->
     case consolectl() of
	 ConsoleCtl when is_pid(ConsoleCtl) ->
	     ConsoleCtl ! Msg;
	 _ ->
	     {error, bad_session}
     end.


inctl() ->
    case get(macsess) of
	#macsess{inctl=InCtl} ->
	    InCtl;
	_ ->
	    undefined
    end.

inctl(Msg) ->
    case inctl() of
	InCtl when is_pid(InCtl) ->
	    InCtl ! Msg;
	_ ->
	    {error, bad_session}
    end.





scrnctl_loop_init(Screen) ->
    receive 
	#macsess{} = Session ->
	    put(macsess, Session),
	    case catch scrnctl_loop_(Screen) of
		{'EXIT', Reason} ->
		    ?dbg("scrnctl EXIT: ~p", [Reason]),
		    exit(Reason);
		_ ->
		    ok
	    end
    end.
	  
scrnctl_loop_(#screen{hx=X, wy=Y, nxy=NXY, next_wid=_NWID} = Screen) ->
    receive 
	{acmd, Cmd} ->
	    {_R, NScreen} = handle_cmd(Cmd, Screen),
	    scrnctl_loop_(NScreen);
	{cmd, From, Ref, Cmd} ->
	    {R, NScreen} = handle_cmd(Cmd, Screen),
	    From ! {Ref, R},
	    scrnctl_loop_(NScreen)
    after 
	50 ->
	    {ok,NX} = io:rows(),
	    {ok,NY} = io:columns(),
	    
	    NScreen = 
		case {NX,NY} of
		    {X,Y} ->
			Screen#screen{nxy=undefined};
		    _ ->
			case NXY of
			    {NX,NY,0} ->
				%% stabilized, notify resize
				?cdbg("Resize ~p x ~p", [NX, NY]),
				{_,NScreen0} = handle_cmd({scresize,NX,NY}, Screen),
				
				NScreen0#screen{nxy=undefined};
			    {NX,NY,N} ->
				?cdbg("I Size ~p ~p x ~p", [N, NX, NY]),
				Screen#screen{nxy={NX,NY,N-1}};
			    _ ->
				%% first noticed resize, wait until stable for 200ms before update
				?cdbg("New Size ~p x ~p", [NX, NY]),
				Screen#screen{nxy={NX,NY,4}}
			end
		end,

	    scrnctl_loop_(NScreen)
    end.

handle_cmd({newwin,X,Y0,OX,OY,WAttrs}, #screen{wy=SY, windows=Windows, next_wid=NWID} = Screen) ->
    View = [], %% lists:duplicate(X, []),
    
    {Y,MaxY} = 
	case Y0 of
	    max ->
		%% size requested as max, allow resize to max
		{SY-OY+1, max};
	    _ ->
		{Y0,Y0}
	end,

    Show = proplists:get_value(show,WAttrs,true),
    Scroll = proplists:get_value(scroll,WAttrs,true),
    Wrap = proplists:get_value(wrap,WAttrs,true),
    Buff = proplists:get_value(buff_size,WAttrs,100),
    VAttrs = proplists:get_value(vattrs,WAttrs,[]),
    
    ClStr = lists:duplicate(Y,32),
    ORefr = 
	lists:map(fun(N) ->
			  %% [{mv,SX+N-1,1},{csi,"0K"}]
			  [{mv,OX+N-1,OY},{sgr, VAttrs, ClStr},{csi,"0m"}]
		  end,
		  lists:seq(1,X)),
    out(lists:flatten(ORefr)),

    Win = #window{id=NWID, scx=OX, scy=OY, x=X, y=Y, maxy=MaxY, hxd=0, fxd=0, cx=1, cy=1, view=View, 
		  show=Show, scroll=Scroll, wrap=Wrap, buff=Buff, buff_scoff=0, attrs=VAttrs},

    ets:insert(Windows, Win),
    NScreen = Screen#screen{next_wid=NWID+1},
    
    {NWID, NScreen};
handle_cmd({whide, W}, #screen{windows=Windows} = Screen) ->
    ?cdbg("whide ~p", [W]),
    case ets:lookup(Windows, W) of
	[#window{show=false} = _Win] ->
	    %% already hidden
	    {ok, Screen};
	[#window{} = Win] ->
	    ets:insert(Windows, Win#window{show=false, need_update=true}),
	    {ok, Screen};
	[] ->
	    {{error, no_such_window}, Screen}
    end;
handle_cmd({wshow, W}, #screen{windows=Windows} = Screen) ->
    ?cdbg("wshow ~p", [W]),
    case ets:lookup(Windows, W) of
	[#window{show=true} = _Win] ->
	    {ok, Screen};
	[#window{} = Win] ->
	    ets:insert(Windows, Win#window{show=true, need_update=true}),
	    {ok, Screen};
	[] ->
	    {{error, no_such_window}, Screen}
    end;
handle_cmd({wresize, W, NX, NY0}, #screen{wy=SY, windows=Windows} = Screen) ->
    ?cdbg("wresize ~p ~p ~p", [W, NX, NY0]),

    case ets:lookup(Windows, W) of
	[#window{x=X, y=_Y, scy=OY, show=Show, view=View,buff=BuffN} = Win] ->
	    {NY,MaxY} = 
		case NY0 of
		    max ->
			%% size requested as max, allow resize to max
			{SY-OY+1, max};
		    _ ->
			{NY0,NY0}
		end,

	    ?dbg("wresize ~p ~p ~p", [W, NX, NY]),


	    NView =
		if 
		    NX > X ->
			%% increased height 
			View;
		    X > NX ->
			%% decreased height
			
			case get_max_line(View) of
			    ML when ML > NX ->
				xy_str_scroll(View,ML-NX,BuffN);
			    _ ->
				View
			end;
		    true ->
			View
		end,
	    [{LX,LY,_Attr,_LStr} | _] = NView,

	    ets:insert(Windows, Win#window{cx=LX, cy=LY, x=NX, y=NY, maxy=MaxY, need_update=Show, view=NView}),
	    {ok, Screen};
	[] ->
	    {{error, no_such_window}, Screen}
    end;
handle_cmd({wkill, W}, #screen{windows=Windows} = Screen) ->
    ?cdbg("wkill ~p", [W]),
    case ets:lookup(Windows, W) of
	[#window{} = Win] ->
	    ets:delete(Windows, Win);
	[] ->
	    {{error, no_such_window}, Screen}
    end;
	    
handle_cmd({wget_xy, W}, #screen{windows=Windows} = Screen) ->
    ?cdbg("wget_xy ~p", [W]),
    case ets:lookup(Windows, W) of
	[#window{x=X, y=Y} = _Win] ->
	    {{ok, {X,Y}}, Screen};
	[] ->
	    {{error, no_such_window}, Screen}
    end;
handle_cmd({wget_cxy, W}, #screen{windows=Windows} = Screen) ->
    ?cdbg("wget_cxy ~p", [W]),
    case ets:lookup(Windows, W) of
	[#window{cx=CX, cy=CY} = _Win] ->
	    {{ok, {CX,CY}}, Screen};
	[] ->
	    {{error, no_such_window}, Screen}
    end;
handle_cmd({w2scr_xy, W, WX, WY}, #screen{windows=Windows} = Screen) ->
    case ets:lookup(Windows, W) of
	[#window{scx=ScX, scy=ScY, hxd=HD} = _Win] ->
	    ?dbg("w2scr_xy ~p ~p ~p -> ~p ~p", [W, WX, WY,ScX+WX-1,ScY+WY-1]),
	    {{ok, {ScX+WX+HD-1,ScY+WY-1}}, Screen};
	[] ->
	    {{error, no_such_window}, Screen}
    end;

handle_cmd({scresize,NX,NY}, #screen{hx=X, wy=Y, windows=Windows} = Screen) ->
    ?cdbg("scresize ~px~p -> ~px~p", [X, Y, NX, NY]),
    csi("2J"),
    Wins = ets:tab2list(Windows),
    lists:foreach(fun(#window{id=W, y=WY, maxy=MaxY} = Win) ->
			  if 
			      WY > NY ->
				  ets:insert(Windows, Win#window{y=NY});
			      NY > WY andalso MaxY==max ->
				  ets:insert(Windows, Win#window{y=NY});
			      true ->
				  ets:insert(Windows, Win#window{y=min(NY,MaxY)})
			  end,
			  handle_cmd({wrefresh,W},Screen)
		  end,
		  Wins),

    {ok, Screen#screen{hx=NX, wy=NY}};


handle_cmd({wclear,W}, #screen{windows=Windows} = Screen) ->
    ?dbg("wclear ~p", [W]),
    case ets:lookup(Windows, W) of
	[#window{show=Show} = Win] ->
	    View = [], 
	    %% only update if show==true
	    NWin = Win#window{cx=1, cy=1, need_update=Show, view=View, buff_scoff=0},
	    ets:insert(Windows, NWin),
	    {ok, Screen};
	_ ->
	    {error, Screen}
    end;

handle_cmd({wmv,W,X,Y}, #screen{windows=Windows} = Screen) ->
    case ets:lookup(Windows, W) of
	[#window{} = Win] ->
	    NWin = Win#window{cx=X, cy=Y},
	    ets:insert(Windows, NWin),
	    {ok, Screen};
	[] ->
	    {error, Screen}
    end;


handle_cmd({wsetattr,W,Attrs}, #screen{windows=Windows} = Screen) ->
    case ets:lookup(Windows, W) of
	[#window{} = Win] ->
	    NWin = Win#window{attrs=Attrs},
	    ets:insert(Windows, NWin),
	    {ok, Screen};
	[] ->
	    {error, Screen}
    end;

handle_cmd({wkilleol,W}, #screen{windows=Windows} = Screen) ->
    case ets:lookup(Windows, W) of
	[#window{y=Width, cx=CX, cy=CY, attrs=Attrs0, view=View0} = Win] ->
	    Attrs = dflt(Attrs0, []),
	    KillLen = Width-CY+1,
	    KillStr = lists:duplicate(KillLen,32),
	    ?dbg("wkilleol W ~p X ~p Y ~p Len ~p", [W, CX, CY, KillLen]),
	    NWin = update_view_xy(CX, CY, KillStr, Attrs, Win),

	    ets:insert(Windows, NWin#window{cx=CX, cy=CY}),
	    {ok, Screen};
	[] ->
	    {error, Screen}
    end;
handle_cmd({wmvaddstr,W,X,Y,Str}, #screen{windows=Windows} = Screen) ->
    case ets:lookup(Windows, W) of
	[#window{attrs=Attrs0} = Win] ->
	    %% AStr = attrib_str(Attrs, Str),
	    %%?dbg("wmvaddstr ~p ~p ~p ~p  ~p", [W, X, Y, Str, View]), 
	    ?dbg("wmvaddstr ~p ~p ~p ~p ", [W, X, Y, Str]), 
	    Attrs = dflt(Attrs0, []),
	    NWin = update_view_xy(X, Y, Str, Attrs, Win),
	    ?dbg("... : ~p", [NWin#window.view]),
	    ets:insert(Windows, NWin),
	    {ok, Screen};
	[] ->
	    {error, Screen}
    end;
handle_cmd({wmvaddattrstr,W,X,Y,Attrs,Str}, #screen{windows=Windows} = Screen) ->
    case ets:lookup(Windows, W) of
	[#window{} = Win] ->
	    %% AStr = attrib_str(Attrs, Str),
	    %%?dbg("wmvaddstr ~p ~p ~p ~p  ~p", [W, X, Y, Str, View]), 
	    ?dbg("wmvaddstr ~p ~p ~p ~p ", [W, X, Y, Str]), 
	    NWin = update_view_xy(X, Y, Str, Attrs, Win),
	    ?dbg("... : ~p", [NWin#window.view]),
	    ets:insert(Windows, NWin),
	    {ok, Screen};
	[] ->
	    {error, Screen}
    end;
handle_cmd({waddstr,W,Str}, #screen{windows=Windows} = Screen) ->
    case ets:lookup(Windows, W) of
	[#window{cx=CX, cy=CY, view=_View} = Win] ->
	    %% ?dbg("waddstr ~p ~p ~p ~p ~p", [W, CX, CY, Str, View]), 
	    ?dbg("waddstr ~p ~p ~p ~p", [W, CX, CY, Str]), 
	    %% ?dbg("SX ~p SY ~p CX ~p CY ~p", [SX, SY, CX, CY]),
	    NWin = update_view_xy(CX, CY, Str, Win),
	    %%?dbg("... ~p ~p ~p", [NWin#window.view,NWin#window.cx,NWin#window.cy]),
	    ets:insert(Windows, NWin),
	    {ok, Screen};
	[] ->
	    {error, Screen}
    end;
handle_cmd({waddattrstr,W,Attrs,Str}, #screen{windows=Windows} = Screen) ->
    case ets:lookup(Windows, W) of
	[#window{cx=CX, cy=CY} = Win] ->
	    %%?dbg("waddstr ~p ~p ~p ~p ~p", [W, CX, CY, Str, View]), 
	    ?dbg("waddstr ~p ~p ~p ~p", [W, CX, CY, Str]), 
	    %% ?dbg("SX ~p SY ~p CX ~p CY ~p", [SX, SY, CX, CY]),
	    NWin = update_view_xy(CX, CY, Str, Attrs, Win),
	    %%?dbg("... ~p ~p ~p", [NWin#window.view,NWin#window.cx,NWin#window.cy]),
	    ets:insert(Windows, NWin),
	    {ok, Screen};
	[] ->
	    {error, Screen}
    end;
handle_cmd({wscrollup,W,N}, #screen{windows=Windows} = Screen) ->
    case ets:lookup(Windows, W) of
	[#window{x=Height, buff=Buff, show=Show, buff_scoff=ScrollOffs0, view=View} = Win] ->
	    Low = 
		lists:foldl(fun({X,_,_,_}, Min) when X<Min ->
				    X;
			       (_, Min) ->
				    Min
			    end,
			    Height, 
			    View),
	    
	    if Low < 1 ->
		    %% has offscreen content
		    ScrollOffs = dflt(ScrollOffs0, 0),
		    NScrollOffs = min(min(Buff, ScrollOffs+N),-(Low)+1),
		    ets:insert(Windows, Win#window{need_update=Show, buff_scoff=NScrollOffs});
	       true ->
		    ok
	    end,

	    {ok, Screen};
	_ ->
	    {error, Screen}
    end;

handle_cmd({wscrolldown,W,N}, #screen{windows=Windows} = Screen) ->
    case ets:lookup(Windows, W) of
	[#window{buff_scoff=ScrollOffs0, show=Show} = Win] ->
	    ScrollOffs = dflt(ScrollOffs0, 0),
	    NScrollOffs = max(0, ScrollOffs-N),
	    ets:insert(Windows, Win#window{need_update=Show, buff_scoff=NScrollOffs}),
	    {ok, Screen};
	_ ->
	    {error, Screen}
    end;


handle_cmd({wscrollbottom,W}, #screen{windows=Windows} = Screen) ->
    case ets:lookup(Windows, W) of
	[#window{buff_scoff=OldOff, show=Show} = Win] ->
	    ets:insert(Windows, Win#window{need_update=Show, buff_scoff=0}),
	    {{ok,OldOff}, Screen};
	_ ->
	    {error, Screen}
    end;

	    

handle_cmd({wrefresh,W}, #screen{selected=Sel, windows=Windows} = Screen) ->
    %% ?dbg("wrefresh ~p", [W]),
    case ets:lookup(Windows, W) of
	[#window{scx=SX, scy=SY, x=Height, y=Y, attrs=Attrs0, show=true, need_update=DoUpdate, hxd=HD, buff_scoff=ScrollOffs0} = Win] ->
	    %% ?dbg("wrefresh ~p ~p", [W, Win]),
	    IsSel = Sel==W, 
	    pheader(IsSel, Win),
	    
	    case DoUpdate of
		true ->
		    ?dbg("need_update ~p", [W]),
		    ClStr = lists:duplicate(Y,32),
		    Attrs = dflt(Attrs0, []),
		    ORefr = 
			lists:map(fun(N) ->
					  %% [{mv,SX+N-1,1},{csi,"0K"}]
					  [{mv,SX+N-1,SY},{sgr, Attrs, ClStr},{csi,"0m"}]
				  end,
				  lists:seq(1+HD,Height)),
		    %% ?dbg("orefr ~p", [ORefr]),
		    out(lists:flatten(ORefr));
		_ ->
		    ok
	    end,
	    
	    ScrollOffs = dflt(ScrollOffs0, 0),
	    
	    View = opt_wrap_lines(Win),

	    Out = 
		lists:map(fun({StX,_StY,_Str}) when (StX+ScrollOffs) < 1 ->
				  %% scroll buffer, not visible
				  [];
			     ({StX,_StY,_Str}) when (StX+ScrollOffs+HD) > Height ->
				  %% scroll buffer, not visible
				  [];
			     ({StX,StY,Str}) ->
				  [{mv,SX+StX+HD+ScrollOffs-1,SY+StY-1},{p,Str}];
			     
			     ({StX,_StY,_Attrs,_Str}) when (StX+ScrollOffs) < 1 ->
				  %% scroll buffer, not visible
				  [];
			     ({StX,_StY,_Attrs,_Str}) when (StX+ScrollOffs+HD) > Height ->
				  %% scroll buffer, not visible
				  [];
			     ({StX,StY,Attrs,Str0}) ->
				  SGR=mksgr(Attrs),
				  Reset=mksgr([]),
				  SLen = length(Str0),
				  %% cut too long lines
				  Str =
				      case (StY+SLen) > Y of
					  true ->
					      lists:sublist(Str0,1,Y-StY+1);
					  _ ->
					      Str0
				      end,
				  [{mv,SX+StX+HD+ScrollOffs-1,SY+StY-1},{csi,SGR},{p,Str},{csi,Reset}]
			  end,
			  lists:reverse(View)),
	    %%?dbg("...out ~p", [Out]),
	    ets:insert(Windows, Win#window{need_update=false}),
	    
	    out(lists:flatten(Out)),
	    {ok, Screen};
	[#window{scx=SX,scy=SY, show=false, x=Height, y=Width, attrs=Attrs0, need_update=true} = Win] ->
	    ClStr = lists:duplicate(Width,32),
	    Attrs = dflt(Attrs0, []),
	    ORefr = 
		lists:map(fun(N) ->
				  %% [{mv,SX+N-1,1},{csi,"0K"}]
				  [{mv,SX+N-1,SY},{sgr,Attrs,ClStr},{csi,"0m"}]
			  end,
			  lists:seq(1,Height)),
	    out(lists:flatten(ORefr)),

	    ets:insert(Windows, Win#window{need_update=false}),
	    
	    {ok, Screen};
	    
	[#window{show=false}] ->
	    {ok, Screen};
	_ ->
	    {error, Screen}
    end;

handle_cmd({wheader,W,Hdr}, #screen{selected=Sel, windows=Windows} = Screen) ->
    case ets:lookup(Windows, W) of
	[#window{} = Win] ->
	    NWin = Win#window{header=Hdr, hxd=1},
	    IsSel = (Sel == W),
	    pheader(IsSel, NWin),
	    ets:insert(Windows, NWin),
	    {ok, Screen};
	[] ->
	    {error, Screen}
    end;
handle_cmd({wfooter,W,Ftr}, #screen{windows=Windows} = Screen) ->
    case ets:lookup(Windows, W) of
	[#window{} = Win] ->
	    NWin = Win#window{footer=Ftr, fxd=1},
	    pfooter(false, NWin),
	    ets:insert(Windows, NWin),
	    {ok, Screen};
	[] ->
	    {error, Screen}
    end;
handle_cmd({wselect, W}, #screen{selected=Sel, sel_pid=SelPid, windows=Windows} = Screen) ->
    case ets:lookup(Windows, Sel) of
	[#window{} = CSW] ->
	    deselect(CSW);
	_ ->
	    ok
    end,
    
    NSelPid = 
	case ets:lookup(Windows, W) of
	    [#window{cpid=NSP} = NSW] ->
		select(NSW),
		NSP;
	    _ ->
		SelPid
	end,
    
    {ok, Screen#screen{selected=W, sel_pid=NSelPid}};

handle_cmd(get_selected, #screen{selected=Sel} = Screen) ->
    {Sel, Screen};
handle_cmd({wpid, W, Pid}, #screen{selected=Sel, sel_pid=SelPid, windows=Windows} = Screen) ->
    NSelPid = 
	case Sel of
	    W ->
		Pid; 
	    _ ->
		SelPid
	end,
    
    case ets:lookup(Windows, W) of
	[#window{} = Win] ->
	    NWin = Win#window{cpid=Pid},
	    ets:insert(Windows, NWin),
	    {ok, Screen#screen{sel_pid=NSelPid}};
	_ ->
	    {ok, Screen}
    end;

handle_cmd({wargs, _W, _Args}, #screen{windows=_Windows} = Screen) ->
    %% nyi
    {ok, Screen};

    
handle_cmd({char, C}, #screen{} = Screen) ->
    sc_handle_char(C, Screen);

handle_cmd({to_clipboard, S}, #screen{} = Screen) ->
    {ok, Screen#screen{clipboard=S}};

handle_cmd(from_clipboard, #screen{clipboard=S} = Screen) ->
    {S, Screen};

handle_cmd(_, Screen) ->
    {error, Screen}.



sc_handle_char([3], Screen) ->
    %% CTRL-c
    inctl(stop),
    {stop, Screen};

sc_handle_char([12], #screen{windows=Windows} = Screen) ->
    %% CTRL-L refresh screen
    csi("2J"),
    Wins = ets:tab2list(Windows),
    lists:foreach(fun(#window{id=W}) ->
			  handle_cmd({wrefresh,W},Screen)
		  end,
		  Wins),
    {ok, Screen};
    
sc_handle_char([23], #screen{windows=Windows} = Screen) ->
    %% CTRL-w log all windows
    Wins = ets:tab2list(Windows),
    lists:foreach(fun(#window{} = Win) ->
			  ?dbg("~p", [Win])
		  end,
		  Wins),
    {ok, Screen};

sc_handle_char([18], Screen) ->
    %% CTRL-r
    out([{rcp, self()}]),
    receive 
	{ccp, CX, CY} ->
	    ?cdbg("rcp CX ~p CY ~p", [CX, CY])
    after 
	1000 ->
	    ?cdbg("NO CX CY", [])
    end,
	    
    {ok, Screen};

sc_handle_char([9], #screen{selected=Sel, windows=Wins} = Screen) ->
    %% TAB, next window
    WList0 = ets:tab2list(Wins), 
    WList = [W || W = #window{id=WId, show=Show} <- WList0, WId==Sel orelse Show],
    Next = 
	case lists:dropwhile(fun(#window{id=WId}) ->
				     Sel /= WId
			     end, 
			     WList) of
	    [] ->
		%% next is first
		[#window{id=Wid} | _] = WList,
		Wid;
	    [_] ->
		%% next is first
		[#window{id=Wid} | _] = WList,
		Wid;
	    [_, #window{id=Wid} | _] ->
		Wid
	end,
    

    handle_cmd({wselect,Next}, Screen);

sc_handle_char(C, #screen{sel_pid=undefined} = Screen) ->
    sc_handle_char2(C, Screen);
sc_handle_char(C, #screen{selected=SelWin, sel_pid=SelPid} = Screen) ->
    SelPid ! {char, SelWin, C},
    {ok, Screen}.


sc_handle_char2([$q], Screen) ->
    {stop, Screen};
sc_handle_char2(_C, Screen) ->
    {ok, Screen}.

	  

select(#window{header=undefined}) ->
    ok;
select(#window{show=true} = Win) ->
    pheader(true, Win),
    pfooter(true, Win);
select(_) ->
    ok.

    

deselect(#window{header=undefined}) ->
    ok;
deselect(#window{show=true} = Win) ->
    pheader(false, Win),
    pfooter(false, Win);
deselect(_) ->
    ok.



pheader(_Selected, #window{header=undefined} = _Win) ->
    ok;
pheader(Selected, #window{header=hline, y=W} = Win) ->
    pheader(Selected, Win#window{header=lists:duplicate(W,$_)});
pheader(true, #window{scx=SX, scy=SY, show=true, header=Hdr, y=Y, buff_scoff=ScOff}) ->
    ScInd =
	case ScOff of
	    0 ->
		"";
	    _ ->
		%% unicode arrow down
		[16#25bc]
		%%"V "
	end,
    Fill = lists:duplicate(Y-length(Hdr)-length(ScInd), 32),
    out([{mv,SX,SY},{csi, "37;40m~s~s~ts", [Hdr, Fill, ScInd]}, {csi, "0m"}]);
pheader(false, #window{scx=SX, scy=SY, show=true, header=Hdr, y=Y, buff_scoff=ScOff}) ->
    ScInd =
	case ScOff of
	    0 ->
		"";
	    _ ->
		%% unicode arrow down
		[16#25bc,32]
		%% "V "
	end,
    Fill = lists:duplicate(Y-length(Hdr)-length(ScInd), 32),
    out([{mv,SX,SY},{csi, ";47m~s~s~ts", [Hdr, Fill, ScInd]}, {csi, "0m"}]);
pheader(_, _) ->
    ok.


pfooter(_Selected, #window{footer=undefined} = _Win) ->
    ok;
pfooter(Selected, #window{footer=hline, y=W} = Win) ->
    pfooter(Selected, Win#window{footer=lists:duplicate(W,32)});
pfooter(true, #window{scx=SX, scy=SY, show=true, footer=Ftr, x=X, y=Y}) ->
    Fill = lists:duplicate(Y-length(Ftr), 32),
    out([{mv,SX+X-1,SY},{csi, "37;40m~s~s", [Ftr, Fill]}, {csi, "0m"}]);
pfooter(false, #window{scx=SX, scy=SY, show=true, footer=Ftr, x=X, y=Y}) ->
    Fill = lists:duplicate(Y-length(Ftr), 32),
    out([{mv,SX+X-1,SY},{csi, ";47m~s~s", [Ftr, Fill]}, {csi, "0m"}]);
pfooter(_, _) ->
    ok.


opt_wrap_lines(#window{x=MaxX, wrap=true, view=View0, buff=BuffN} = Win) ->
    View = lists:keysort(1, lists:reverse(View0)),
    case opt_wrap_lines2(View, 0, Win, []) of
	{0,NView} ->
	    NView; 
	{NWr, NView} ->
	    %%?dbg("View ~p", [View]),
	    %%?dbg("NView ~p ~p", [NWr, NView]),

	    ML =get_max_line(NView),
	    if ML > MaxX ->
		    NView2=xy_str_scroll(NView, NWr, BuffN),
		    %% ?dbg("NView2 ~p", [NView2]),
		    NView2;
	       true ->
		    NView
	    end
    end;
opt_wrap_lines(#window{view=View}) ->
    View.

opt_wrap_lines2([{StX, StY, Attrs, Str0} | Strs], WrAcc, #window{y=Y} = Win, Acc) ->
    SLen = length(Str0),

	case StY > Y of
	    true ->
		%% beginning of output outside Win, wrap before line
		NStY = StY rem Y,
		NWrAcc = WrAcc + (StY div Y),
		opt_wrap_lines2([{StX,NStY,Attrs,Str0} | Strs], NWrAcc, Win, Acc); 
	    _ ->
		{Str,STail} =
		    case (StY+SLen) > Y of
			true ->
			    {lists:sublist(Str0,1,Y-StY+1),lists:sublist(Str0,Y-StY+2,SLen)};
			_ ->
			    {Str0, []}
		    end,
		    
		    case STail of
			[] ->
			    NAcc = [{StX+WrAcc, StY, Attrs, Str} | Acc],
			    opt_wrap_lines2(Strs, WrAcc, Win, NAcc);
			_ ->
			    NAcc = [{StX+WrAcc, StY, Attrs, Str} | Acc],
			    opt_wrap_lines2([{StX,1,Attrs,STail} | Strs], WrAcc+1, Win, NAcc)
		    end
	end;
opt_wrap_lines2([], WrAcc, _, Acc) ->
    {WrAcc, lists:reverse(Acc)}.




%% {key(), slogan(), type(), validation(), HeadAcc, TailAcc}
read_form(W, Form0) ->
    MaxHL = 
	lists:foldl(fun({_Key,Slogan,_}, L) ->
			    mmax(L, length(Slogan))
		    end,
		    0,
		    Form0),

    VOff = MaxHL + 3,
    
    %% add submit button
    Form = Form0 ++ [{ecursed_form_submit, undefined, {choice, ["Submit", "Cancel"]}}],
    
    InitHdr = 
	fun(_RN,undefined) ->
		lists:duplicate(VOff, 32);
	   (RN,S) ->
		?cdbg("hdr ~p ~p", [RN, S]),
		{sgr, [bold], lists:flatten(io_lib:format("~-*s:  ", [MaxHL, S]))}
		%%wmvaddattrstr(W, RN, 1, [bold], "~-*s:", [MaxHL, S])
	end,
		
    Up = [27,91,65],
    Down = [27,91,66],
    Tab = [9], 
    ShiftTab = [27,91,90],
    Enter = [13],

    SuspendOn = [Up, Down, Tab, ShiftTab, Enter],


    {FormSt,_FRN} = 
	lists:mapfoldl(fun({Key,Slogan,string}, RN) ->
			       Prompt = InitHdr(RN, Slogan),
			       ReadStr = init_read_str(W,RN,1,Prompt,[],[],all,SuspendOn),
			       {{RN, Key, Slogan, string, ReadStr}, RN+1};
			  ({Key,Slogan,{string,Default}}, RN) ->
			       Prompt = InitHdr(RN, Slogan),
			       ReadStr = init_read_str(W,RN,1,Prompt,lists:reverse(Default),[],all,SuspendOn),
			       {{RN, Key, Slogan, string, ReadStr}, RN+1};
			  ({Key,Slogan,integer}, RN) ->
			       Prompt = InitHdr(RN, Slogan),
			       ReadStr = init_read_str(W,RN,1,Prompt,[],[],lists:seq($0,$9),SuspendOn),
			       {{RN, Key, Slogan, integer, ReadStr}, RN+1};
			  ({Key,Slogan,{choice, Choices}}, RN) ->
			       ?cdbg("choice ~p", [Key]),
			       Prompt = InitHdr(RN, Slogan),
			       Select=init_select(W,RN,1,Prompt,Choices,SuspendOn,1,1),
			       {{RN, Key, Slogan, select, Select}, RN+1}
		       end,
		       1,
		       Form),

    p_read_form(FormSt),
    
    %% wrefresh(W),
    %% wmvaddstr(W,ScX+FRN-1,1,"Submit:"),
    %% FormSt = FormSt0 ++ [{FRN,submit,"Submit",submit,{select, FRN, VOff, ["Submit", "Cancel"],1,1}}],
    
    
    FormCtl = #form{form=FormSt, focus_field=1},

    case read_form_loop(W, FormCtl) of
	{ok, #form{form=RForm0}} ->
	    RForm =
		lists:map(fun({_,Key,_,_,#read_str{head=HAcc,tail=TAcc}}) ->
				  {Key, lists:reverse(HAcc)++TAcc};
			     ({_,Key,_,_,#select{choices=Choices,curr_sel=SelVal}}) ->
				  Val = lists:nth(SelVal, Choices),
				  {Key, Val}
			  end,
			  RForm0),
	    {ok, RForm};
	Err ->
	    Err
    end.


p_read_form(Form) ->	    
    lists:foreach(fun({RN, Key, Slogan, _, #read_str{} = ReadStr}) ->
			  p_read_str(ReadStr, false);
		     ({RN, Key, Slogan, _, #select{} = Select}) ->
			  p_select(Select, false);
		     (_) ->
			  ok
		  end,
		  Form).

    

read_form_loop(W, #form{form=FormWdgs, form_cb=_FormCB, focus_field=FocusField} = FormCtl) ->
    MaxFF = length(FormWdgs),
    Up = [27,91,65],
    Down = [27,91,66],
    Tab = [9], 
    ShiftTab = [27,91,90],
    Enter = [13],
    
    ?cdbg("Form ~p FF ~p", [FormCtl, FocusField]),    
    
    FormField = 
	case lists:keysearch(FocusField, 1, FormWdgs) of
	    {value, FF} ->
		FF;
	    false ->
		undefined
	end,

    ?cdbg("FF ~p", [FormField]),
    receive after 10 -> ok end, 
    
    Res = 
	case FormField of
	    {_, _Key, _Slogan, _Type, #read_str{} = ReadStr} ->
		sc_read_str(ReadStr);
	    {_, ecursed_form_submit, _Slogan, _, Select} ->
		SubmitSuspendOn = [Up, ShiftTab],
		sc_select(Select#select{suspend_on=SubmitSuspendOn});
	    {_, _Key, _Slogan, _Type, Select} ->
		sc_select(Select);
	    undefined ->
		undefined
	end,
    
    ?cdbg("Res ~p", [Res]),
    receive after 10 -> ok end,
    
    case FocusField of
	MaxFF ->
	    case Res of
		{ok,"Submit"} ->
		    {ok, FormCtl};
		{ok,"Cancel"} ->
		    cancel;
		{suspend, _Char, #select{} = NSelect} ->
		    NFF = 
			case FormField of
			    {N, Key, Slogan, Type, #select{}} ->
				{N, Key, Slogan, Type, NSelect};
			    _ ->
				FormField
			end,
		    NFormWdgs = lists:keyreplace(FocusField, 1, FormWdgs, NFF),
		    read_form_loop(W, FormCtl#form{form=NFormWdgs, focus_field=FocusField-1});
		cancel ->
		    read_form_loop(W, FormCtl#form{focus_field=FocusField-1})
	    end;
	_ ->
	    
	    {Action, NewFormField} = 
		case Res of
		    {suspend, Char, #read_str{} = NReadStr} ->
			?cdbg("RStr ~p", [NReadStr]),
			NFF = 
			    case FormField of
				{N, Key, Slogan, Type, #read_str{}} ->
				    {N, Key, Slogan, Type, NReadStr};
				_ ->
				    FormField
			    end,
			{Char, NFF};
		    {suspend, Char, #select{} = NSelect} ->
			NFF = 
			    case FormField of
				{N, Key, Slogan, Type, #select{}} ->
				    {N, Key, Slogan, Type, NSelect};
				_ ->
				    FormField
			    end,

			{Char, NFF};
		    cancel ->
			%% nyi
			{Down, FormField};
		    Unknown ->
			?cdbg("UNKNOWN ~p", [Unknown]),
			{Down, FormField}
			    
		end,
	    
	    NFormWdgs = lists:keyreplace(FocusField, 1, FormWdgs, NewFormField),
	    
	    ?cdbg("Action ~p", [Action]),
	    receive after 10 -> ok end,

	    case Action of
		cancel ->
		    {ok, FormCtl};
		ok ->
		    NFormCtl = FormCtl#form{form=NFormWdgs, focus_field=FocusField+1},
		    read_form_loop(W, NFormCtl);
		Prev when Prev==Up; Prev==ShiftTab; Prev==prev ->
		    if FocusField==1 ->
			    read_form_loop(W, FormCtl);
		       true ->
			    NFormCtl = FormCtl#form{focus_field=FocusField-1},
			    read_form_loop(W, NFormCtl)
		    end;
		Next when Next==Down; Next==Tab; Next==Enter; Next==next ->
		    NFormCtl = FormCtl#form{focus_field=FocusField+1},
		    read_form_loop(W, NFormCtl)
	    end
    end.
		       
			    
read_str(W,Prompt) ->
    {ok,{X,Y}} = wget_cxy(W),
    read_str(W,X,Y,Prompt,[]).


read_str(W, Prompt, Head) ->
    {ok,{X,Y}} = wget_cxy(W),
    read_str(W,X,Y,Prompt, Head).

read_str(W, WX, WY, Prompt, Head) ->
    read_str(W, WX, WY, Prompt, Head, [], all, []).

read_str(W, WX, WY, Prompt, Head, Tail, Filter, SuspendOn) ->
    ReadStr = init_read_str(W, WX, WY, Prompt, Head, Tail, Filter, SuspendOn),
    
    read_str(ReadStr).

read_str(#read_str{} = ReadStr) ->
    case sc_read_str(ReadStr) of
	{suspend, _, _} = Res ->
	    Res;
	Res ->
	    p_read_str(ReadStr, clear),
	    %% out([{mv,ScX,ScY},{csi,"0K"}]),
	    Res
    end.


init_read_str(W, WX, WY, Prompt, Head, Tail, Filter, SuspendOn) ->
    ?cdbg("init_read_str...", []),
    %% {ok, {ScX, ScY}} = w2scr_xy(W,WX,WY),
    #read_str{window=W, x=WX, y=WY, prompt=Prompt, head=Head, tail=Tail, filter=Filter, suspend_on=SuspendOn}.


read_str_modal(W, WX, WY, Prompt, Head, Tail, Filter, SuspendOn) ->
    ?cdbg("rs3", []),
    Ref = make_ref(), 
    
    {ok, {ScX, ScY}} = w2scr_xy(W,WX,WY),

    ReadStr = init_read_str(screen, ScX, ScY, Prompt, Head, Tail, Filter, SuspendOn),

    %% inloop ! {read_str, ScX, ScY, Prompt, [{mv,ScX,ScY}, {p,Prompt}, " "], Head, self(), Ref},
    inctl({ReadStr, self(), Ref}),
    
    receive 
	{string, Ref, {suspend, _, _} = Res} ->
	    %% dont clear suspended input field
	    Res;
	{string, Ref, Res} ->
	    out([{mv,ScX,ScY},{csi,"0K"}]),
	    Res
    end.

read_str_select(W, WX, WY, Prompt, Head, Choises) ->
    Ref = make_ref(), 
    
    {ok, {ScX, ScY}} = w2scr_xy(W,WX,WY),
    ?cdbg("read_str/5 ~p ~p ~p ~p", [WX,WY,ScX,ScY]),

    %% inloop ! {read_str, ScX, ScY, Prompt, [{mv,ScX,ScY}, {p,Prompt}, " "], Head, self(), Ref},
    inctl({read_str_select, ScX, ScY, Prompt, Head, Choises, self(), Ref}),
    
    receive 
	{string, Ref, Str} ->
	    wmv(W,WX,WY),
	    wkilleol(W),
	    Str
    end.

yn(W,X,Y,Prompt) ->
    yn(W,X,Y,Prompt,true).

yn(W,WX,WY,Prompt,Sel) ->
    {CSel,VPos} = 
	case Sel of
	    true ->
		{1,1};
	    false ->
		{2,2}
	end,

    case select(W,WX,WY,Prompt,["Yes", "No"], [], CSel, VPos) of
	{ok, "Yes"} ->
	    true;
	_ ->
	    false
    end.
	
    
select(W,WX,WY,Prompt,Choices) ->
    select(W,WX,WY,Prompt,Choices, []).
select(W,WX,WY,Prompt,Choices, SuspendOn) ->
    select(W,WX,WY,Prompt,Choices, SuspendOn,1,1).
select(W,WX,WY,Prompt,Choices, SuspendOn,CSel,VPos) ->    
    Select = #select{window=W, x=WX, y=WY, 
		     prompt=Prompt, 
		     choices=Choices, 
		     suspend_on=SuspendOn, 
		     curr_sel=CSel, 
		     vpos=VPos}, 

    case sc_select(Select) of
	{suspend, _, _} = Resp ->
	    Resp;
	Resp ->
	    wmv(W,WX,WY),
	    wkilleol(W),

	    Resp
    end.

init_select(W,WX,WY,Prompt,Choices, SuspendOn,CSel,VPos) ->
    ?cdbg("init_select...", []),
    Select = #select{window=W, x=WX, y=WY, 
		     prompt=Prompt, 
		     choices=Choices, 
		     suspend_on=SuspendOn, 
		     curr_sel=CSel, 
		     vpos=VPos}, 
    
    Select.




select_modal(W,WX,WY,Prompt,Choices, SuspendOn,CSel,VPos) ->
    Ref = make_ref(),
    {ok, {ScX, ScY}} = w2scr_xy(W,WX,WY),
    
    Select = init_select(screen, ScX, ScY, Prompt,Choices, SuspendOn,CSel,VPos),
    inctl(Select),
    
    receive 
	{select, Ref, {suspend, _, _} = Resp} ->
	    Resp;
	{select, Ref, Resp} ->
	    out([{mv,ScX,ScY},{csi,"0K"}]),
	    Resp
    end.




-record(cstate, {win, msgfilter}).

consoleloop_init() ->
    receive 
	#macsess{} = Session ->
	    ?dbg("console init", []),
	    put(macsess, Session),
	    consoleloop(undefined)
    end.
	    
consoleloop(undefined) ->
    receive 
	{window, W} ->
	    ?dbg("console win ~p", [W]),
	    %% consoleloop(#cstate{win=W, msgfilter=[dbg,msg,err]})
	    consoleloop(#cstate{win=W, msgfilter=[msg,err]})
    end;
consoleloop(#cstate{win=W, msgfilter=Filter} = State) ->
    receive
	{char, W, [$c]} ->
	    %% clear
	    wclear(W),
	    wrefresh(W),
	    consoleloop(State);
	{char, W, [$d]} ->
	    %% toggle dbg
	    NFilter = 
		case lists:member(dbg, Filter) of
		    true ->
			Filter--[dbg];
		    _ ->
			[dbg | Filter]
		end,
	    waddstr(W,"toggle dbg -> ~p", [NFilter]),
	    consoleloop(State#cstate{msgfilter=NFilter});
	{char, W, [27,91,65]} ->
	    %% up, scroll window
	    
	    ecursed:wscrollup(W),
	    ecursed:wrefresh(W),
	    consoleloop(State);
	{char, W, [27,91,66]} ->
	    %% down, scroll info window
	    
	    ecursed:wscrolldown(W),
	    ecursed:wrefresh(W),
	    consoleloop(State);
	{char, W, [13]} ->
	    %% Enter, scroll to bottom
	    ecursed:wscrollbottom(W),
	    ecursed:wrefresh(W),
	    consoleloop(State);
	{msg, Lvl, Loc, S} ->
	    DoShow = 
		case Lvl of
		    dbg ->
			lists:member(dbg,Filter);
		    _ ->
			true
		end,

	    case DoShow of
		true ->
		    ULoc =
			case lists:member(dbg, Filter) of
			    true ->
				Loc;
			    _ ->
				""
			end,
		    
		    US = ULoc++S,
		    
		    waddstr(W,US),
		    wrefresh(W),
		    consoleloop(State);
		_ ->
		    consoleloop(State)
	    end
    end.



rcp() ->	    
    out([{rcp, self()}]),
    receive 
	{ccp, CX, CY} ->
	    {CX, CY}
    after 200 ->
	    {undefined,undefined}
    end.
		

%% drv_loop
%% read (receive) data from input, forward to inctl
drv_loop(P, InCtl) ->
    receive
	{_, {data, Cs}} ->
	    InCtl ! {char, drv, Cs},
	    drv_loop(P, InCtl)
    end.


inloop_init() ->
    receive 
	#macsess{} = Session ->
	    put(macsess, Session),
	    case catch inloop() of
		{'EXIT', Reason} ->
		    ?dbg("inctl EXIT: ~p", [Reason]),
		    exit(Reason);
		_ ->
		    ok
	    end
    end.


inloop() ->
    receive 
	{char, drv, [27,91 | T] = Cs} ->
	    ?cdbg("Char(s) ~w", [Cs]),
	    case lists:reverse(T) of
		[$R | _] ->
		    %% cursor report
		    {CX, CY} = parse_cursor_report(T),
		    receive 
			{rcp, To} ->
			    %% out([{mv,40,1},{fa,"cl rcp ~p -> ~p", [Cs, To]}]),
			    To ! {ccp, CX,CY}
		    after 100 ->
			    ok
		    end,
		    inloop();
		_ -> 
		    ascrnctl({char, Cs}),
		    inloop()
	    end;
	{char, drv, [C]} ->
	    ?cdbg("Char ~w", [C]),
	    ascrnctl({char, [C]}),
	    inloop();
	{char, drv, Cs} ->
	    ?cdbg("Chars ~w", [Cs]),
	    TokCh = tok_chs(Cs), 
	    lists:foreach(fun(C) ->
				  ascrnctl({char, C})
			  end, 
			  TokCh),
	    inloop();
	{#read_str{} = ReadStr, From, Ref} ->
	%% {read_str, ScX, ScY, Prompt, Head, Tail, Filter, SuspendOn, From, Ref} ->
	    ?cdbg("Read str...", []),
	    Res = sc_read_str(ReadStr), 
	    ?cdbg("...Str: ~p", [Res]),
	    From ! {string, Ref, Res},
	    inloop();
	{read_str_select, ScX, ScY, Prompt, Head, Choices, From, Ref} ->
	    Str = sc_read_str_select(ScX, ScY, Prompt, Head, [], Choices, undefined),
	    %%csi("?25l"),
	    ?cdbg("...Str: ~p", [Str]),
	    From ! {string, Ref, Str},
	    inloop();
	{yn, ScX, ScY, Prompt, Sel, From, Ref} ->
	    Resp = sc_yn(ScX, ScY, Prompt, Sel),
	    From ! {yn, Ref, Resp},
	    inloop();
	{select, ScX, ScY, Prompt, Choices, SuspendOn, CSel, VPos, From, Ref} ->
	    Resp = sc_select(ScX, ScY, Prompt, Choices, CSel, VPos, SuspendOn), 
	    From ! {select, Ref, Resp},
	    inloop();
	stop ->
	    ok
    end.

tok_chs([27,91 | TCs]) ->
    %% CSI
    tok_csi(TCs, [91,27]);
tok_chs([C | Cs]) ->
    [[C] | tok_chs(Cs)];
tok_chs([]) ->
    [].


tok_csi([C | Cs], Acc) ->
    case lists:member(C, [$A, $B, $C, $D, $E, $F, $G, $H, $J, $K, $S, $T, $f, $m, $n, $s, $u ,$l, $h]) of
	true ->
	    [lists:reverse([C | Acc]) | tok_chs(Cs)];
	false ->
	    tok_csi(Cs, [C | Acc])
    end;
tok_csi([], []) ->
    %% Unterminated CSI 
    [];
tok_csi([], Acc) ->
    %% Unterminated CSI 
    [lists:reverse(Acc)].



    


%% parse_cursor_report
%% 
parse_cursor_report(T) ->
    pcr1(T,[]).

pcr1([$; | Cs], XAcc) ->
    pcr2(Cs, [], list_to_integer(lists:reverse(XAcc)));
pcr1([D | Cs], XAcc) ->
    pcr1(Cs, [D | XAcc]).
    
pcr2([$R], YAcc, X) ->
    {X, list_to_integer(lists:reverse(YAcc))};
pcr2([D | Cs], YAcc, X) ->
    pcr2(Cs, [D | YAcc], X).



sc_read_str_select(ScX, ScY, Prompt, HeadAcc, TailAcc, Choices, CurrSel) ->
    CurrStr = lists:reverse(HeadAcc)++TailAcc,

    MatchChoices = filter_choices(CurrStr, Choices),
    ?cdbg("CS ~p", [CurrSel]),
    receive after 10 -> ok end,
    ChLen = length(MatchChoices), 
    
    %% If CurrSel=undefined, we are in text entry mode, otherwise in selection mode

    case CurrSel of
	undefined ->
	    %% Text entry mode
	    
	    OF = 
		fun() ->
			sc_out([{mv,ScX, ScY},{p,Prompt},{csi,"0K"}]),
			sc_out([lists:reverse(HeadAcc)]),
			case TailAcc of
			    [] ->
				sc_out([{csi,";47m"}," ",{sgr,[], "  "}]);
			    [PTC | PTCs] ->
				sc_out([{csi, ";47m"}, [PTC], {csi, "0m"}, PTCs, {p, "   "}])
			end,
			
			ShowChoices = lists:sublist(MatchChoices, 1, 3),
			?cdbg("ShCh ~p", [ShowChoices]),
			[sc_out([{sgr, [faint], Choice}, "  "]) || Choice <- ShowChoices],
			
			if ChLen > 3 ->
				sc_out([{p, "..."}]);
			   true ->
				ok
			end

		end,
	    
	    out(OF);
	
	{CSel,_CVPos} ->
	    {ShSt, ShEnd} = 
		case CurrSel of
		    {1,_} ->
			{1, mmin(ChLen,3)};
		    {_,1} ->
			{CSel,mmin(ChLen,CSel+2)};
		    {_,2} ->
			{CSel-1,mmin(ChLen,CSel+1)};
		    {_,_} ->
			%% {CurrSel-1, mmin(ChLen,CurrSel+1)}
			{mmax(1,CSel-2),CSel}
			    
		end,
	    
	    SelVals0 = lists:sublist(MatchChoices, ShSt, ShEnd-ShSt+1),
	    
	    SelVals = lists:zip(lists:seq(ShSt, ShEnd), SelVals0),
	    
	    OF = fun() ->
			 sc_out([{mv,ScX,ScY},{p,Prompt},{csi,"0K"},{sgr, [faint],CurrStr}, {sgr, [reset], "   "}]),
			 
			 case ShSt of 
			     1 ->
				 ok;
			     _ ->
				 sc_out(["..."])
			 end,
			 
			 Suff = 
			     case ShEnd of
				 ChLen ->
				     "";
				 _ ->
				     "..."
			     end,
			 
			 lists:foreach(fun({CIN, SVal}) when CSel == CIN ->
					       case CIN of
						   ShEnd ->
						       sc_out([{sgr,[bold,{bg,white}],SVal}, {sgr, [],Suff}]);
						   _ ->
						       sc_out([{sgr,[bold,{bg,white}],SVal}, {sgr, [], "  "}])
					       end;
					  ({CIN, SVal}) ->
					       case CIN of
						   ShEnd ->
						       sc_out([{sgr, [], SVal},{sgr,[], Suff}]);
						   _ ->
						       sc_out([{sgr, [], SVal++"  "}])
					       end
				       end,
				       SelVals)
		 end,
	    out(OF)
    end,
    

    case CurrSel of
	undefined ->
	    %% entry mode
	    receive 
		{char, _, "\n"} ->
		    {ok, lists:reverse(HeadAcc)++TailAcc};
		{char, _, "\r"} ->
		    {ok, lists:reverse(HeadAcc)++TailAcc};
		{char, _, [27]} ->
		    %% ESC
		    cancel;
		{char, _, [3]} ->
		    %% CTRL-C
		    cancel;
		{char, _, [9]} ->
		    %% TAB
		    %% expand nyi
		    sc_read_str_select(ScX, ScY, Prompt, HeadAcc, TailAcc, Choices, CurrSel);
		{char, _, [27,91,68]} ->
		    %% left arrow
		    case HeadAcc of
			[] ->
			    sc_read_str_select(ScX, ScY, Prompt, [], TailAcc, Choices, CurrSel);
			[HC | HT]  ->
			    sc_read_str_select(ScX, ScY, Prompt, HT, [HC | TailAcc], Choices, CurrSel)
		    end;
		{char, _, [27,91,67]} ->
		    %% right arrow
		    case TailAcc of
			[] ->
			    %% enter select mode
			    sc_read_str_select(ScX, ScY, Prompt, HeadAcc, [], Choices, {1,1});
			[TC | TT]  ->
			    sc_read_str_select(ScX, ScY, Prompt, [TC | HeadAcc], TT, Choices, undefined)
		    end;
		{char, _, [127]} ->
		    case HeadAcc of
			[] ->
			    sc_read_str_select(ScX, ScY, Prompt, [], TailAcc, Choices, CurrSel);
			[_HC | HT]  ->
			    sc_read_str_select(ScX, ScY, Prompt, HT, TailAcc, Choices, CurrSel)
		    end;
		{char, _, [1]} ->
		    %% ctrl-a
		    sc_read_str_select(ScX, ScY, Prompt, [], lists:reverse(HeadAcc)++TailAcc, Choices, CurrSel);
		{char, _, [5]} ->
		    %% ctrl-e
		    sc_read_str_select(ScX, ScY, Prompt, lists:reverse(TailAcc)++HeadAcc, [], Choices, CurrSel);
		{char, _, [11]} ->
		    %% ctrl-k
		    sc_read_str_select(ScX, ScY, Prompt, HeadAcc, [], Choices, CurrSel);
		    
		{char, _, [21]} ->
		    %% ctrl-u
		    sc_read_str_select(ScX, ScY, Prompt, [], TailAcc, Choices, CurrSel);
		    
		{char, _, [C]} ->
		    sc_read_str_select(ScX, ScY, Prompt, [C | HeadAcc], TailAcc, Choices, CurrSel)
	    end;
	{CSel2, VPos2} ->
	    %% select mode
	    receive 
		{char, _, [27,91,68]} -> 
		    %% left
		    ?cdbg("...left", []),
		    case CSel2-1 of 
			0 ->
			    %% back to entry mode
			    sc_read_str_select(ScX, ScY, Prompt, HeadAcc, TailAcc, Choices, undefined);
			NSel ->
			    case VPos2 of
				1 ->
				    sc_read_str_select(ScX, ScY, Prompt, HeadAcc, TailAcc, Choices, {NSel,1});
				_ ->
				    sc_read_str_select(ScX, ScY, Prompt, HeadAcc, TailAcc, Choices, {NSel,VPos2-1})
			    end
		    end;
		{char, _, [27,91,67]} -> 
		    %% right
		    ?cdbg("...right", []),
		    NSel = mmin(ChLen, CSel2+1),
		    ?cdbg("NSel ~p", [NSel]),
		    case VPos2 of
			3 ->
			    sc_read_str_select(ScX, ScY, Prompt, HeadAcc, TailAcc, Choices, {NSel,3});
			_ -> 
			    NVPos = mmin(VPos2+1, ChLen),
			    sc_read_str_select(ScX, ScY, Prompt, HeadAcc, TailAcc, Choices, {NSel,NVPos})
		    end;
		{char, _, [13]} ->
		    {CSel2,_} = CurrSel,
		    SelVal = lists:nth(CSel2,MatchChoices),
		    ?cdbg("...selected: ~p", [SelVal]),
		    %% go back to entry mode with selected value
		    sc_read_str_select(ScX, ScY, Prompt, lists:reverse(SelVal), [], Choices, undefined);
		{char, _, [27]} ->
		    %% ESC
		    ?cdbg("...cancel", []),
		    sc_read_str_select(ScX, ScY, Prompt, HeadAcc, TailAcc, Choices, undefined);
		{char, _, [3]} ->
		    %% CTRL-C
		    ?cdbg("...CTRL-C", []),
		    cancel;
		{char, _, C} ->
		    %% "eat" all char events
		    ?cdbg("...~w", [C]),
		    sc_read_str_select(ScX, ScY, Prompt, HeadAcc, TailAcc, Choices, CurrSel)
	    end
    end.

    



filter_choices(Str, Choices) ->
    case re:compile(Str) of
	{ok, RE} ->
	    lists:filter(fun(Choice) ->
				 case re:run(Choice, RE) of
				     {match, _} ->
					 true;
				     _ ->
					 false
				 end
			 end,
			 Choices);
	_ ->
	    lists:filter(fun(Ch) ->
				 is_substr(Str, Ch)
			 end,
			 Choices)
    end.
    
	    
is_substr([C | Cs] = Str, [C | Ch]) ->
    case lists:prefix(Cs, Ch) of
	true ->
	    true;
	false ->
	    is_substr(Str, Ch)
    end;
is_substr(Str, [_ | Ch]) ->
    is_substr(Str, Ch);
is_substr(_Str, []) ->
    false.


    
    



sc_read_str(ScX, ScY, Prompt, HeadAcc, TailAcc, Filter, SuspendOnChar) ->
    sc_read_str({screen, ScX, ScY}, Prompt, HeadAcc, TailAcc, Filter, SuspendOnChar).

sc_read_str({window, W, WX, WY}, Prompt, HeadAcc, TailAcc, Filter, SuspendOnChar) ->
    sc_read_str(#read_str{window=W, x=WX, y=WY, prompt=Prompt, head=HeadAcc, tail=TailAcc, filter=Filter, suspend_on=SuspendOnChar}).

sc_read_str(#read_str{head=HeadAcc, tail=TailAcc, filter=Filter, suspend_on=SuspendOnChar} = ReadStr) ->
    ?cdbg("sc_read_str ~p ~p", [Filter, SuspendOnChar]),
    
    %% p_read_str(WScr, Prompt, HeadAcc, TailAcc, Filter, true),    
    p_read_str(ReadStr, true),    
    
    receive 
	{char, _, [13]} ->
	    case lists:member([13], SuspendOnChar) of
		true ->
		    p_read_str(ReadStr, false),    
		    %% p_read_str(WScr, Prompt, HeadAcc, TailAcc, Filter, false),
		    
		    {suspend, [13], ReadStr}; 
		false ->
		    {ok, lists:reverse(HeadAcc)++TailAcc}
	    end;
	{char, _, [27]} ->
	    %% ESC
	    cancel;
	{char, _, [3]} ->
	    %% CTRL-C
	    cancel;
	{char, _, [9]} ->
	    %% TAB
	    %% expand nyi
	    sc_read_str(ReadStr);
	{char, _, [11]} ->
	    %% ctrl-k
	    p_read_str(ReadStr, clear_value),
	    to_clipboard(TailAcc),
	    sc_read_str(ReadStr#read_str{tail=[]});
	{char, _, [25]} ->
	    %% ctrl-y
	    NHeadAcc = 
		case from_clipboard() of
		    undefined ->
			HeadAcc;
		    PStr0 ->
			PStr = 
			    case Filter of
				undefined ->
				    PStr0; 
				all ->
				    PStr0; 
				_ ->
				    lists:filter(fun(C) -> lists:member(C,Filter) end, PStr0)
			    end,
			
			lists:reverse(PStr)++HeadAcc
		end,
	    sc_read_str(ReadStr#read_str{head=NHeadAcc});
	{char, _, [21]} ->
	    %% ctrl-u
	    sc_read_str(ReadStr#read_str{head=[]});
	{char, _, [1]} ->
	    %% ctrl-a
	    sc_read_str(ReadStr#read_str{head=[], tail=lists:reverse(HeadAcc)++TailAcc});
	{char, _, [5]} ->
	    %% ctrl-e
	    sc_read_str(ReadStr#read_str{head=lists:reverse(TailAcc)++HeadAcc, tail=[]});
	{char, _, [27,91,68]} ->
	    %% left arrow
	    case HeadAcc of
		[] ->
		    sc_read_str(ReadStr);
		[HC | HT]  ->
		    sc_read_str(ReadStr#read_str{head=HT, tail=[HC | TailAcc]})
	    end;
	{char, _, [27,91,67]} ->
	    %% right arrow
	    case TailAcc of
		[] ->
		    sc_read_str(ReadStr);
		[TC | TT]  ->
		    sc_read_str(ReadStr#read_str{head=[TC | HeadAcc], tail=TT})
	    end;
	{char, _, [127]} ->
	    %% backspace
	    case HeadAcc of
		[] ->
		    sc_read_str(ReadStr);
		[_HC | HT]  ->
		    sc_read_str(ReadStr#read_str{head=HT})
	    end;
	{char, _, [C] = Data} ->
	    case lists:member(Data, SuspendOnChar) of
		true ->
		    %% remove possible cursor
		    p_read_str(ReadStr, false),    

		    %% p_read_str(WScr, Prompt, HeadAcc, TailAcc, Filter, false),
		    
		    {suspend, Data, ReadStr};
		false ->
		    if C > 31 ->
			    UseChar = 
				case Filter of
				    _ when is_list(Filter) ->
					lists:member(C, Filter);
				    _ ->
					true
				end,

			    case UseChar of 
				true ->
				    sc_read_str(ReadStr#read_str{head=[C | HeadAcc]});
				_ ->
				    %% XXX visual bell
				    sc_read_str(ReadStr)
			    end;
					
		       true ->
			    sc_read_str(ReadStr)
		    end
	    end;
	{char, _, Data} ->
	    case lists:member(Data, SuspendOnChar) of
		true ->
		    p_read_str(ReadStr, false),    

		    %% p_read_str(WScr, Prompt, HeadAcc, TailAcc, Filter, false),
		    {suspend, Data, ReadStr};
		false ->
		    sc_read_str(ReadStr)
	    end
    end.


%% p_read_str({screen, ScX, ScY}, Prompt, HeadAcc, TailAcc, Filter, Focus) ->
p_read_str(#read_str{window=screen, x=ScX, y=ScY, head=HeadAcc, tail=TailAcc, prompt=Prompt} = ReadStr, Focus) ->
    ?cdbg("p_read_str scr...", []),
    OF = 
	fun() ->
		sc_out([{mv,ScX, ScY},Prompt,{sgr,[reset],""},{csi,"0K"}]),
		sc_out([lists:reverse(HeadAcc)]),
		case Focus of
		    true ->
			%% output cursor
			case TailAcc of
			    [] ->
				sc_out([{csi,";47m"}," ",{csi, "0m"}]);
			    [PTC | PTCs] ->
				sc_out([{csi, ";47m"}, [PTC], {csi, "0m"}, PTCs])
			end;
		    false ->
			sc_out([TailAcc])
		end
	end,
		
    out(OF);

p_read_str(#read_str{window=W, x=WX, y=WY, prompt=Prompt} = ReadStr, clear) ->
    %% clear form
    p_read_str(ReadStr, clear_value),

    SPrompt =
	case Prompt of
	    {sgr, SGR, F} ->
		lists:flatten(F);
	    {sgr, SGR, F, A} ->
		lists:flatten(io_lib:format(F,A));
	    _ ->
		lists:flatten(Prompt)
	end,
    
    Len = length(SPrompt),
    OW = lists:duplicate(Len+1, 32),
    
    wmvaddstr(W,WX,WY,OW),
    wrefresh(W);
    
p_read_str(#read_str{window=W, active=IsActive0, x=WX, y=WY, prompt=Prompt, head=HeadAcc, tail=TailAcc}, Focus) ->
    ?cdbg("p_read_str w...", []),
    {PrAttr, PrF, PrA} = 
	case Prompt of
	    {sgr, SGR, F} ->
		{SGR,F,[]};
	    {sgr, SGR, F, A} ->
		{SGR,F,A};
	    _ ->
		{[], Prompt, []}
	end,
    
    IsActive = dflt(IsActive0, true),

    case IsActive0 of
	false ->
	    wmvaddattrstr(W,WX,WY,[faint],PrF,PrA),
	    waddattrstr(W, [faint], lists:reverse(HeadAcc)),
	    waddattrstr(W, [faint], TailAcc++" ");
	
	_ ->
	    wmvaddattrstr(W,WX,WY,PrAttr,PrF,PrA),
	    waddstr(W, lists:reverse(HeadAcc)),
	    
	    case Focus of
		true ->
		    %% output cursor
		    case TailAcc of
			[] ->
			    waddattrstr(W,[{bg,white}]," ");
			[PTC | PTCs] ->
			    waddattrstr(W,[{bg,white}],[PTC]),
			    waddstr(W,PTCs++" ")
		    end;
		false ->
		    waddstr(W,TailAcc++" ");
		clear_value ->
		    Len = length(HeadAcc)+length(TailAcc),
		    OW = lists:duplicate(Len+1, 32),
		    waddstr(W,OW)
	    end
    end,
    wrefresh(W).
		


sc_yn(ScX,ScY,Prompt,Sel) ->    
    out([{mv,ScX,ScY},{p,Prompt},{csi,"0K"}]),
    case Sel of
	true ->
	    out([{sgr,[bold,{bg,white}],"Yes"}, {sgr, []," No"}]);
	_ ->
	    out([{sgr,[],"Yes "}, {sgr, [bold,{bg,white}],"No"},{sgr, [reset], ""}])
    end,
    
    receive 
	{char, _, [27,91 |_]} -> 
	    %% arrow
	    NSel = not Sel,
	    sc_yn(ScX,ScY,Prompt,NSel);
	{char, _, [13]} ->
	    Sel;
	{char, _, [27]} ->
	    %% ESC
	    cancel;
	{char, _, [3]} ->
	    %% CTRL-C
	    cancel;
	{char, _, [Yes]} when Yes==$y; Yes==$Y ->
	    true;
	{char, _, [No]} when No==$n; No==$N ->
	    false;
	{char, _, _} ->
	    %% "eat" all char events
	    sc_yn(ScX,ScY,Prompt,Sel)
    end.


sc_select(ScX, ScY, Prompt, Choices, CSel, VPos, SuspendOn) ->
    Select = #select{window=screen, x=ScX, y=ScY, 
		     prompt=Prompt, 
		     choices=Choices, 
		     suspend_on=SuspendOn, 
		     curr_sel=CSel, 
		     vpos=VPos}, 
    
    sc_select(Select).


sc_select(#select{curr_sel=CurrSel, vpos=VPos, choices=Choices, suspend_on=SuspendOnChar} = Select) ->
    ?cdbg("select ~p", [Select]),    
    
    ChLen = length(Choices), 

    p_select(Select, true),
    
    receive 
	{char, _, [27,91,68]} -> 
	    %% left
	    ?cdbg("...left", []),
	    case CurrSel of
		1 ->
		    sc_select(Select);
		_ ->
		    case VPos of
			1 ->
			    sc_select(Select#select{curr_sel=CurrSel-1});
			_ ->
			    sc_select(Select#select{curr_sel=CurrSel-1, vpos=VPos-1})
		    end
	    end;
	{char, _, [27,91,67]} -> 
	    %% right
	    ?cdbg("...right", []),
	    case CurrSel of
		ChLen ->
		    sc_select(Select);
		_ ->
		    case VPos of
			4 ->
			    sc_select(Select#select{curr_sel=CurrSel+1});
			_ ->
			    sc_select(Select#select{curr_sel=CurrSel+1, vpos= VPos+1})
		    end
	    end;
	{char, _, [13]} ->
	    case lists:member([13], SuspendOnChar) of
		true ->
		    p_select(Select, false),
		    {suspend, [13], Select};
		_ ->
		    SelVal = lists:nth(CurrSel,Choices),
		    ?cdbg("...selected: ~p", [SelVal]),
		    {ok,SelVal}
	    end;
	{char, _, [27]} ->
	    %% ESC
	    ?cdbg("...cancel", []),
	    cancel;
	{char, _, [3]} ->
	    %% CTRL-C
	    ?cdbg("...CTRL-C", []),
	    cancel;
	{char, _, C} ->
	    case lists:member(C, SuspendOnChar) of
		true ->
		    p_select(Select, false),
		    {suspend, C, Select};
		_ ->
		    ?cdbg("...~w", [C]),
		    %% Find choice starting with C
		    LChoices = [string:to_lower(Choice) || Choice <- Choices],
		    LNChoices = lists:zip(lists:seq(1,length(LChoices)), LChoices), %% -> [{1, "choise"} ...
		    LCh = string:to_lower(C),
		    
		    %% Search first in choices following
		    {Prev, Foll} = lists:split(CurrSel, LNChoices),
		    
		    case match_choice_prefix(LCh, Foll) of 
			{NSel, _} ->
			    sc_select(Select#select{curr_sel=NSel});
			undefined ->
			    case match_choice_prefix(LCh, Prev) of 
				{NSel, _} ->
				    sc_select(Select#select{curr_sel=NSel, vpos=1});
				undefined ->
				    sc_select(Select)
			    end
		    end
	    end
    end.
    
match_choice_prefix(P, [{N, S} = Ch | Chs]) ->
    case lists:prefix(P,S) of
	true ->
	    Ch;
	_ ->
	    match_choice_prefix(P, Chs)
    end;
match_choice_prefix(_, []) ->
    undefined.


p_select(#select{window=Window, x=X, y=Y, prompt=Prompt, choices=Choices, curr_sel=CurrSel, vpos=VPos}, Focus) ->
    ?cdbg("p_select ~p ~p ~p", [Window, X, Y]),
    ChLen = length(Choices), 
    
    {ShSt, ShEnd} = 
	case {CurrSel, VPos} of
	    {1, _} ->
		{1, mmin(ChLen,4)};
	    {ChLen, _} ->
		{mmax(1,CurrSel-3),ChLen};
	    {_, 1} ->
		{CurrSel, mmin(ChLen,CurrSel+3)};
	    {_, 2} ->
		{CurrSel-1, mmin(ChLen,CurrSel+2)};
	    {_, 3} ->
		{CurrSel-2, mmin(ChLen,CurrSel+1)};
	    _ ->
		{CurrSel-3, CurrSel}
	end,
        
    SelVals0 = lists:sublist(Choices, ShSt, ShEnd-ShSt+1),
    
    SelVals1 = lists:zip(lists:seq(ShSt, ShEnd), SelVals0),
    
    Pref = 
	case ShSt of 
	    1 ->
		[];
	    _ ->
		[prefix]
	end,

    Suff = 
	case ShEnd of
	    ChLen ->
		[];
	    _ ->
		[suffix]
	end,

    SelVals2 = 
	lists:map(fun({CSel, SVal0}) when CSel == CurrSel ->
			  {SVal, SVAttr} = 
			      case SVal0 of
				  {SV,SAFun} when is_function(SAFun) ->
				      {SV, SAFun(SV)};
				  {_, _} -> 
				      SVal0;
				  _ ->
				      {SVal0, []}
			      end,
			  {CSel, SVal, SVAttr, true};
		     ({CSel, SVal0}) ->
			  {SVal, SVAttr} = 
			      case SVal0 of
				  {SV,SAFun} when is_function(SAFun) ->
				      {SV, SAFun(SV)};
				  {_, _} -> 
				      SVal0;
				  _ ->
				      {SVal0, []}
			      end,
			  {CSel, SVal, SVAttr, false}
		  end,
		  SelVals1),
    
    SelVals3 = interleave(SelVals2, sep),
 
    SelVals = Pref++SelVals3++Suff,
    
    
    case Window of
	_ when Window==screen; Window==undefined ->
	    OF = 
		fun() ->
			sc_out([{mv,X,Y},Prompt,{csi,"0K"}]),
			
			lists:foreach(fun({CSel, SVal, Attr, true}) ->
					      case Focus of
						  true ->
						      sc_out([{sgr,[bold,{bg,white}]++Attr,SVal},{sgr,[reset],""}]);
						  _ ->
						      sc_out([{sgr,[bold]++Attr,SVal},{sgr,[reset],""}])
					      end;
					 ({CSel, SVal, Attr, _}) ->
					      case Focus of
						  true ->
						      sc_out([{sgr,[]++Attr,SVal}]);
						  _ ->
						      sc_out([{sgr,[faint]++Attr,SVal},{sgr,[reset],""}])
					      end;
					 (prefix) ->
					      %% sc_out(["...  "]);
					      sc_out([16#2026, 32]);
					 (suffix) ->
					      sc_out([32, 16#2026]);
					      %% sc_out(["  ..."]);
					 (sep) ->
					      sc_out(["  "])
				      end,
				      SelVals)
		end,
	    
	    out(OF);
	_ ->
	    %% use window buffer
	    {PrAttr, PrF, PrA} = 
		case Prompt of
		    {sgr, SGR, F} ->
			{SGR,F,[]};
		    {sgr, SGR, F, A} ->
			{SGR,F,A};
		    _ ->
			{[], Prompt, []}
		end,
	    
	    
	    wmvaddattrstr(Window,X,Y,PrAttr, PrF, PrA),
	    wkilleol(Window),
	    lists:foreach(fun({CSel, SVal, Attr, true}) ->
				  case Focus of
				      true ->
					  waddattrstr(Window, [bold,{bg,white}]++Attr, SVal);
				      _ ->
					  waddattrstr(Window, [bold]++Attr, SVal)
				  end;
			     ({CSel, SVal, Attr, _}) ->
				  case Focus of
				      true ->
					  waddattrstr(Window, Attr, SVal);
				      _ ->
					  waddattrstr(Window, [faint]++Attr, SVal)
				  end;
			     (prefix) ->
				  %% waddstr(Window, "...  ");
				  waddstr(Window, [16#2026, 32]);
			     (suffix) ->
				  %% waddstr(Window, "  ...");
				  waddstr(Window, [32, 16#2026]);
			     (sep) ->
				  waddstr(Window, "  ")
			  end,
			  SelVals),
	    wrefresh(Window)
    end.
	    
    



csi(F,A) ->
    out([{csi,F,A}]).

csi(S) ->
    out([{csi, S}]).


	    
out(L) when is_list(L) ->
    case get(macsess) of
	#macsess{outctl=OutCtl} ->
	    OutCtl ! {out, L};
	_ ->
	    {error, bad_session}
    end;
out(F) when is_function(F) ->
    case get(macsess) of
	#macsess{outctl=OutCtl} ->
	    OutCtl ! {out, F};
	_ ->
	    {error, bad_session}
    end.


outloop() ->
    receive 
	{out, L} when is_list(L) ->
	    sc_out(L);
	{out, F} when is_function(F) ->
	    F()
    end,
    outloop().

sc_out(L) when is_list(L) ->
    case io_lib:printable_list(lists:flatten(L)) of
	true ->
	    sc_out({p, L});
	_ ->
	    lists:foreach(fun(I) ->
				  sc_out(I)
			  end,
			  L)
    end;

sc_out({mv,R,C}) ->
    sc_mv(R,C);
sc_out({csi,S}) ->
    sc_csi(S);
sc_out({sgr,Attr,F,A}) ->
    Str = lists:flatten(F,A),
    sc_out({sgr,Attr,Str});
sc_out({sgr,Attr,Str}) ->
    Sgr=mksgr(Attr),
    sc_csi(Sgr),
    io:format("~s", [Str]);

sc_out({csi,F,A}) ->
    sc_csi(F,A);
sc_out({b,S}) ->
    sc_csi("1m~s", [S]),
    sc_csi("0m");
sc_out({r,S}) ->
    sc_csi("7m~s", [S]),
    sc_csi("0m");
sc_out({rcp,To}) ->
    %% report cursor
    inctl({rcp, self()}),
    sc_csi("6n"),
    receive 
	{ccp, CX, CY} ->
	    To ! {ccp, CX, CY}
    after 
	1000 ->
	    error
    end;
sc_out({fa,F,A}) ->
    sc_out({p, io_lib:format(F,A)});
sc_out({p, S}) when is_list(S) ->
    io:format("~ts", [sc_utf8(S)]);
sc_out(_O) ->
    ok.




sc_utf8([S | Cs]) when is_list(S) ->
    [sc_utf8(S) | sc_utf8(Cs)];
sc_utf8([C | Cs]) when is_integer(C), C > 255 ->
    [char_to_utf8(C) | sc_utf8(Cs)];
sc_utf8([C | Cs]) ->
    [C | sc_utf8(Cs)];
sc_utf8([]) ->
    [].



	

%% 
sc_mv(X,Y) ->
    sc_csi("~p;~pH",[X,Y]).

sc_fa(X,Y,F,A) ->
    sc_mv(X,Y),
    %% io:format(F,A).
    sc_out({fa,F,A}).

sc_csi(F,A) ->
    sc_csi(lists:flatten(io_lib:format(F,A))).    
	

sc_csi(S) ->
    %% io:format([27, $[ | S]).
    sc_out({p, [27, $[ | S]}).


fixnl([$\n | Cs]) ->
    [$\r, $\n | fixnl(Cs)];
fixnl([L | Cs]) when is_list(L) ->
    [fixnl(L) | fixnl(Cs)];
fixnl([C | Cs]) ->
    [C | fixnl(Cs)];
fixnl([]) ->
    [].





get_max_line(View) ->
    lists:foldl(fun({SX,_SY,_Attr,_S}, CX) when SX > CX ->
			SX;
		   (_, CX) ->
			CX
		end,
		1,
		View).
    




update_view_xy(X,Y,Str,#window{attrs=Attrs} = Win) ->
    update_view_xy(X,Y,Str,Attrs,Win).

update_view_xy(X,Y,Str,Attrs0,#window{x=WMaxX, show=Show, need_update=DoUpdate, view=View0, scroll=IsScroll, buff=BuffN, buff_scoff=ScOff} = Win) ->
    Attrs = dflt(Attrs0,[]),
    XYStrs = split_xy_str(X,Y,Attrs,Str,Win),
    View = replace_xy_str(XYStrs, View0),
    
    %% find highest line
    SMaxX = get_max_line(XYStrs), 
    
    NView0 = XYStrs ++ View,

    {NView1, NDoUpdate, NScOff} = 
	case {SMaxX > WMaxX, IsScroll} of
	    {true, true} ->
		%% new string writes below last line, scroll
		NLines = SMaxX - WMaxX,
		NOff = 
		    if ScOff == 0 ->
			    0;
		       true ->
			    ScOff+NLines
		    end,
		{xy_str_scroll(NView0, NLines, BuffN), true, NOff};
	    _ ->
		{NView0,DoUpdate,ScOff}
	end,
    
    [{LX,LY,_Attr,LStr} | _] = NView1,
    
    
    
    Win#window{cx=LX, cy=LY+length(LStr), need_update=Show andalso NDoUpdate, view=NView1, buff_scoff=NScOff}.
    
    

%% update_xy(Strs, View).
xy_str_scroll([{X,_Y,_Attr,_Str} | XYS], N, BuffN) when N>=(X+BuffN) ->
    %% scrolled off screen
    xy_str_scroll(XYS, N, BuffN);
xy_str_scroll([{X,Y,Attr,Str} | XYS], N, BuffN) ->
    [{X-N,Y,Attr,Str} | xy_str_scroll(XYS, N, BuffN)];
xy_str_scroll([],_N, _BuffN) ->
    [].


replace_xy_str([XYStr | XYSs], View) ->
    NView = replace_xy_str(XYStr, View),
    replace_xy_str(XYSs, NView);
replace_xy_str([], View) ->
    View;
replace_xy_str({X,Y,_,S} = XYS, [{X, VY, VAttr, VS} = V | VStr]) ->
    SL = length(S),
    VSL = length(VS),
    
    if (Y=<VY) andalso ((Y+SL) >= (VY+VSL)) ->
	    %% XYS overwrites V, remove V
	    replace_xy_str(XYS, VStr);
       (Y=<VY) andalso((Y+SL) > VY) ->
	    %% XYS partially overwrites V (beginning), replace V
	    NVY = Y+SL,
	    [{X,NVY,VAttr,lists:sublist(VS,Y+SL-VY+1,VSL)} | replace_xy_str(XYS, VStr)];
       (Y>VY) andalso ((Y+SL) > (VY+VSL)) ->
	    %% XYS partially overwrites V (end), replace V
	    [{X,VY,VAttr,lists:sublist(VS,1,Y-VY)} | replace_xy_str(XYS, VStr)];
       true ->
	    [V | replace_xy_str(XYS, VStr)]
    end;
replace_xy_str(XYS, [VS | VSs]) ->
    [VS | replace_xy_str(XYS, VSs)];
replace_xy_str(_, []) ->
    [].

    
test_rxys() ->
    V0 = [{1,1,[a],"abcd"},{1,8,[b],"BCDE"}],
    
    replace_xy_str([{1,3,[c],"cdefghi"}], V0).

    



split_xy_str(X,Y,Attr,Str,W) ->
    lists:reverse(split_xy_str(X,Y,Y,Attr,Str,W,[])).

split_xy_str(X,OY,_CY,Attr,[$\n | Cs], #window{} = Win, Acc) ->
    [{X,OY, Attr, lists:reverse(Acc)} | split_xy_str(X+1,1,1,Attr,Cs, Win, [])];
split_xy_str(X,OY,_CY,Attr,[$\r | Cs], #window{} = Win, Acc) ->
    [{X,OY, Attr,lists:reverse(Acc)} | split_xy_str(X,1,1,Attr,Cs, Win, [])];
split_xy_str(X,OY,CY,Attr,[$\t | Cs], #window{} = Win, Acc) ->
    TabSpcs = 8 - (CY rem 8), 
    Tab = lists:duplicate(TabSpcs, 32),
    split_xy_str(X,OY,CY+TabSpcs,Attr,Cs,Win,Tab++Acc);
split_xy_str(X,OY,CY,Attr,[C | Cs], #window{} = Win, Acc) ->
    split_xy_str(X,OY,CY+1,Attr,Cs,Win,[C | Acc]);
split_xy_str(X,OY,_,Attr,[],_Win,Acc) ->
    [{X,OY,Attr,lists:reverse(Acc)}].


update(XYStrs, View) ->
    {_RX, _RY, NView} = update_xy(XYStrs, View),
    NView.

update_xy([{X,Y,Str}], View) ->
    {X, Y+length(Str), update(X,Y,Str,View)};
update_xy([{X,Y,Str} | Ss], View) ->
    NView = update(X,Y,Str,View),
    update_xy(Ss, NView).


%% update(X,Y,Str,View)
update(1,Y,Str,[R | Rs]) ->
    [update_row(Y, Str, R) | Rs];
update(X,Y,Str,[R | Rs]) ->
    [R | update(X-1,Y,Str,Rs)];
update(1,Y,Str,[]) ->
    [update_row(Y, Str, [])];
update(X,Y,Str,[]) ->
    [[] | update(X-1,Y,Str,[])].



update_row(1, Str, R) ->
    case {length(Str), length(R)} of
	{LS, LR} when LS >= LR ->
	    Str;
	_ ->
	    Str ++ lists:sublist(R, length(Str)+1, length(R))
    end;
update_row(N, Str, [RC | RCs]) ->
    [RC | update_row(N-1, Str, RCs)];
update_row(N, Str, []) ->
    [32 | update_row(N-1, Str, [])].



attrib_str([{attrs, Attrs} | Cs]) ->
    CSI=mksgr(Attrs),
    [CSI | attrib_str(Cs)];
attrib_str([C | Cs]) ->
    [C | attrib_str(Cs)];
attrib_str([]) ->
    [].

mksgr([]) ->
    "0m";
mksgr(Attrs) ->
    SgrStr0 = 
	lists:map(fun(Attr) -> 
			  [";" | sgrstr(Attr)]
		  end, 
		  Attrs),
    
    [$; | SgrStr] = lists:flatten(SgrStr0),
    
    SgrStr++"m".

sgrstr({fg,FG}) ->
    "3"++enc_color(FG);
sgrstr({bg,FG}) ->
    "4"++enc_color(FG);
sgrstr(bold) ->
    sgrstr({bold,true});
sgrstr({bold,true}) ->
    "1";
sgrstr({bold,false}) ->
    "22";
sgrstr(reset) ->
    "0";
sgrstr(faint) ->
    sgrstr({faint,true});
sgrstr({faint,true}) ->
    "2";
sgrstr({faint,false}) ->
    "22";
sgrstr(blink) ->
    "5";
sgrstr(ul) ->
    sgrstr(underline);
sgrstr(underline) ->
    "4";
sgrstr(crossed) ->
    "9";
sgrstr(overline) ->
    "53";
sgrstr(normal) ->
    "22";
sgrstr(_) ->
    "".


enc_color(black) ->
    "0";
enc_color(red) ->
    "1";
enc_color(green) ->
    "2";
enc_color(yellow) ->
    "3";
enc_color(blue) ->
    "4";
enc_color(magenta) ->
    "5";
enc_color(cyan) ->
    "6";
enc_color(white) ->
    "7";
enc_color(I) when is_integer(I), 0 >= I, 7 =< I ->
    integer_to_list(I);
enc_color(_) ->
    "0".



dflt(undefined, D) ->
    D;
dflt(V,_) ->
    V.

mmax(A,B) when A>B ->    
    A;
mmax(_,B) -> 
    B.

mmin(A,B) when A<B ->    
    A;
mmin(_,B) -> 
    B.

interleave([], T) ->
    [];
interleave([I], _T) ->
    [I];
interleave([I | Is], T) ->
    [I, T | interleave(Is, T)].




char_to_utf8(Ch) when integer(Ch), Ch >= 0 ->
    if Ch < 128 ->
	    %% 0yyyyyyy
	    [Ch];
       Ch < 16#800 ->
	    %% 110xxxxy 10yyyyyy
	    [16#C0 + (Ch bsr 6),
	     128+(Ch band 16#3F)];
       Ch < 16#10000 ->
	    %% 1110xxxx 10xyyyyy 10yyyyyy
	    if Ch < 16#D800; Ch > 16#DFFF, Ch < 16#FFFE ->
		    [16#E0 + (Ch bsr 12),
		     128+((Ch bsr 6) band 16#3F),
		     128+(Ch band 16#3F)]
	    end;
       Ch < 16#200000 ->
	    %% 11110xxx 10xxyyyy 10yyyyyy 10yyyyyy
	    [16#F0+(Ch bsr 18),
	     128+((Ch bsr 12) band 16#3F),
	     128+((Ch bsr 6) band 16#3F),
	     128+(Ch band 16#3F)];
       Ch < 16#4000000 ->
	    %% 111110xx 10xxxyyy 10yyyyyy 10yyyyyy 10yyyyyy
	    [16#F8+(Ch bsr 24),
	     128+((Ch bsr 18) band 16#3F),
	     128+((Ch bsr 12) band 16#3F),
	     128+((Ch bsr 6) band 16#3F),
	     128+(Ch band 16#3F)];
       Ch < 16#80000000 ->
	    %% 1111110x 10xxxxyy 10yyyyyy 10yyyyyy 10yyyyyy 10yyyyyy
	    [16#FC+(Ch bsr 30),
	     128+((Ch bsr 24) band 16#3F),
	     128+((Ch bsr 18) band 16#3F),
	     128+((Ch bsr 12) band 16#3F),
	     128+((Ch bsr 6) band 16#3F),
	     128+(Ch band 16#3F)]
    end.

    
    



    
	   



