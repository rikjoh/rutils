
%%% _________________________________________________________________________
%%%
%%% Author:           'rikjoh@gmail.com'
%%% _________________________________________________________________________
%%%
%%%
%%%   TODO: <Description of module>
%%%   
%%% _________________________________________________________________________


-hrl_revision(?FILE).

-define(cdbg(F,A), macurse:cmsg(?MODULE, ?LINE, dbg, F, A)).
-define(cmsg(F,A), macurse:cmsg(?MODULE, ?LINE, msg, F, A)).
-define(cerr(F,A), macurse:cmsg(?MODULE, ?LINE, err, F, A)).



%% unicode graphics
-define(mcuc_HL, 16#2500).    %% Horizontal line
-define(mcuc_HHL, 16#2501).   %% Horizontal heavy line
-define(mcuc_VL, 16#2502).    %% Vertical line
-define(mcuc_VHL, 16#2503).   %% Vertical heavy line
-define(mcuc_UL, 16#250c).    %% Upper Left corner
-define(mcuc_HUL, 16#250d).    %% Heavy Upper (normal) Left corner
-define(mcuc_UHL, 16#250e).    %% (normal) Upper Heavy Left corner
-define(mcuc_KUHL, 16#250e).    %% Heavy Upper Heavy Left corner
-define(mcuc_UR, 16#2510).    %% Upper Right corner
-define(mcuc_HUR, 16#2511).    %% Heavy Upper (normal) Right corner
-define(mcuc_UHR, 16#2512).    %% (normal) Upper Heavy Right corner
-define(mcuc_KUHR, 16#2513).    %% Heavy Upper Heavy Right corner

-define(mcuc_LL, 16#2514).    %% Lower Left corner
-define(mcuc_HLL, 16#2515).    %% Heavy Lower (normal) Left corner
-define(mcuc_LHL, 16#2516).    %% (normal) Lower Heavy Left corner
-define(mcuc_HLHL, 16#2517).    %% Heavy Lower Heavy Left corner

-define(mcuc_LR, 16#2518).    %% Lower Right corner
-define(mcuc_HLR, 16#2519).    %% Heavy Lower (normal) Right corner
-define(mcuc_LHR, 16#251a).    %% (normal) Lower Heavy Right corner
-define(mcuc_HLHR, 16#251b).    %% Heavy Lower Heavy Right corner

