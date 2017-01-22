-module(hnmd_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/src/inet_dns.hrl").

-export([
	all/0, groups/0,
	init_per_suite/1, end_per_suite/1,
	init_per_testcase/2, end_per_testcase/2,
	%% group: boot
	test_hnmd_boot/1
]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() -> [
	{group, boot}
].

groups() -> [
		{boot, [], [
			test_hnmd_boot
		]}
	].


init_per_suite(Config) ->
	stop_system(),
	start_system(),
	Config.

end_per_suite(_Config) ->
	stop_system(),
	ok.

init_per_testcase(_Suite, Config) ->
	Config.

end_per_testcase(_Suite, _Config) ->
	ok.

%%%===================================================================
%%% TESTS
%%%===================================================================

%%--------------------------------------------------------------------
%% Group : boot
%%--------------------------------------------------------------------

test_hnmd_boot(_Config) ->
	ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

start_system() ->
	application:ensure_all_started(hnmd_dns),
	application:ensure_all_started(hnmd_dhcp),
	application:ensure_all_started(hnmd_http),
	ok.

stop_system() ->
	application:stop(hnmd_dns),
	application:stop(hnmd_dhcp),
	application:stop(hnmd_http),
	ok.
