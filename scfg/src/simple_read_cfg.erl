
%%% _________________________________________________________________________
%%%
%%% Author:           rikjoh@gmail.com
%%% _________________________________________________________________________
%%%
%%%
%%%   TODO: <Description of module>
%%%   
%%% _________________________________________________________________________


-module(simple_read_cfg).

-revision('$Revision: $ ').
-modified('$Date: $ ').

-export([read/1]).
-export([pp_err/1]).

-define(dbg(F,A), io:format("[~p:~p] "++F++"\n", [?MODULE, ?LINE | A])).

read(File) ->
    RWF = fun(_) -> false end,
    case file:read_file(File) of
	{ok, Bin} ->
	    {ok, Scan, _Ls} = erl_scan:string(binary_to_list(Bin), {1,1}, [{reserved_word_fun,RWF}]),
	    parse(Scan);
	Error ->
	    Error
    end.

pp_err(Err) ->
    io:format("~s\n", [f_err(Err)]).


f_err({error, Reason, {L, C}, NextTok}) ->
    NTStr =
	case NextTok of
	    undefined ->
		"";
	    {string, Str} ->
		io_lib:format(" (before ~p)", [Str]);
	    TV ->
		io_lib:format(" (before \"~p\")", [TV])
	end,
    io_lib:format("ERROR (line ~p col ~p) : ~p~s", [L, C, Reason, NTStr]);
f_err(Err) ->
    io_lib:format("ERROR ~p", [Err]).

    

parse(Ts) ->
    parse(Ts, []).

parse([{atom,_L,Param},{'=',_} | PV], Acc) ->
    case parse_value(PV) of
	{ok, Value, T} ->
	    parse(T, [{Param, Value} | Acc]);
	{error, _Reason, _EL, _NT} = Error ->
	    Error
    end;
parse([{',',_} | T], Acc) ->
    %% optional separator between parameters
    parse(T, Acc);
parse([T | _Ts], _Acc) ->
    {error, syntax_error, tline(T), tval(T)};
parse([], Acc) ->
    {ok, lists:reverse(Acc)}.

parse_value([{'#',L},{'(',_} | T]) ->
    case parse_keyvals(T, ')') of
	{ok,PKV,NT} ->
	    {ok,PKV,NT};
	{error, unterminated_keyval, _, _} ->
	    {error, unterminated_keyval, L, undefined};
	Error ->
	    Error
    end;
parse_value([{'#',L},{atom,_,Name},{'{',_} | T]) ->
    case parse_keyvals(T, '}') of
	{ok,PKV,NT} ->
	    {ok,{record, Name, PKV},NT};
	{error, unterminated_keyval, _, _} ->
	    {error, unterminated_record, L, undefined};
	Error ->
	    Error
    end;
parse_value([{'[',L} | LI]) ->
    case parse_list(LI, ']') of
	{ok,PL,NT} ->
	    {ok,PL,NT};
	{error, unterminated_list, _, _} ->
	    {error, unterminated_list, L, undefined};
	Error ->
	    Error
    end;
parse_value([{'{',L} | LI]) ->
    %% tuple
    case parse_list(LI, '}') of
	{ok,PL,NT} ->
	    {ok,list_to_tuple(PL),NT};
	{error, unterminated_list, _, _} ->
	    {error, unterminated_tuple, L, undefined};
	Error ->
	    Error
    end;
parse_value([{atom,_L,A} | T]) ->
    {ok, A, T};
parse_value([{string,_L,S} | T]) ->
    {ok, S, T};
parse_value([{integer,_L,I} | T]) ->
    {ok, I, T};
parse_value([{float,_L,F} | T]) ->
    {ok, F, T};
parse_value([T | _]) ->
    {error, syntax_error, tline(T), tval(T)}.



parse_keyvals(KVs, ClBr) ->
    parse_keyvals(KVs, ClBr, []).
    
parse_keyvals(KVs, ClBr, Acc) ->
    case parse_keyval(KVs) of
	{ok, KV, T} ->
	    case T of
		[{ClBr,_} | NT] ->
		    {ok, lists:reverse([KV | Acc]), NT};
		[{',',_} | NKVs] ->
		    parse_keyvals(NKVs, ClBr, [KV | Acc]);
		[Tok | _] ->
		    {error, syntax_error, tline(Tok), tval(Tok)};
		[] ->
		    {error, unterminated_keyval, undefined, undefined}
	    end;
	Error ->
	    Error
    end.

    


parse_keyval([{atom,_L,K},{'=',_} | V]) ->
    case parse_value(V) of
	{ok, Value, T} ->
	    {ok, {K,Value}, T};
	Error ->
	    Error
    end;
parse_keyval([T,{'=',_} | V]) ->
    {error, invalid_key, tline(T), tval(T)};
parse_keyval([T | _]) ->
    {error, not_key_value, tline(T), tval(T)}.



parse_list(L, ClBr) ->
    parse_list(L, ClBr, []).

parse_list(L, ClBr, Acc) ->
    case parse_value(L) of
	{ok, V, T} ->
	    case T of
		[{ClBr,_} | NT] ->
		    {ok, lists:reverse([V | Acc]), NT};
		[{',',_} | NT] ->
		    parse_list(NT, ClBr, [V | Acc]);
		[Tok | _] ->
		    {error, syntax_error, tline(Tok), tval(Tok)};
		[] ->
		    {error, unterminated_list, undefined, undefined}
	    end;
	Error ->
	    Error
    end.

%% internals
tline({_T,L}) ->
    L;
tline({_T,L,_V}) ->
    L.

tval({V,_L}) ->
    V;
tval({string,_L,V}) ->
    {string, V};
tval({_T,_L,V}) ->
    V.


