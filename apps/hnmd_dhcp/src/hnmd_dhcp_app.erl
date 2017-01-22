%%%-------------------------------------------------------------------
%% @doc hnmd_dhcp public API
%% @end
%%%-------------------------------------------------------------------

-module(hnmd_dhcp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    hnmd_dhcp_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
