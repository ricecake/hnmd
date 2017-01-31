-module(hnmd_util).

%% API exports
-export([]).

%%====================================================================
%% API functions
%%====================================================================

to_lower(String) when is_list(String) -> list_to_binary(string:to_lower(String));
to_lower(String) when is_binary(String) ->
	<< <<(do_lower(Char)):8>> || <<Char:8>> <= String >>.

do_lower(Char) when Char >= 65 andalso Char =< 90 -> Char + 32;
do_lower(Char) -> Char.


%%====================================================================
%% Internal functions
%%====================================================================
