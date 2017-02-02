-module(hnmd_util).

%% API exports
-export([
	to_lower/1
]).
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

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



-ifdef(TEST).

basic_test_() ->
        {"hnmd_util Tests", [
                {"text functions", [
                        {"lowercase helper", [
                                {"works on binary", ?_assertMatch(<<"az!1az">>, to_lower(<<"AZ!1az">>))},
                                {"works on list", ?_assertMatch(<<"az!1az">>, to_lower("AZ!1az"))}
                        ]}
                ]}
        ]}.

-endif.

