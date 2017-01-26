%%@doc DNSplice worker
%%
%% This module is responsible for processing DHCP requests,
%% handing them off to a worker, and routing the replys back to
%% the client.
%%
%%@end
-module(hnmd_dhcp_worker).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
	start_link/1,
	handle/2
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

%% ------------------------------------------------------------------
%% Defines and includes
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%%@doc starts a worker process
%%
%% function that starts a worker process.  Should be called by the
%% hnmd_dhcp worker supervisor.
%%
%%@end

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%%@doc Starts the process of handling a DHCP packet
%%
%% Starts up a DHCP request worker, and starts the process
%% of determining how the packet should appropriately be handled
%%
%%@end

-spec handle(Packet :: binary(), Sender :: {IP :: inet:ip_address(), Port :: inet:port_number()}) -> ok.

handle(Packet, Sender) ->
	{ok, _Pid} = hnmd_dhcp_worker_sup:start_worker(Packet, Sender),
	ok.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({Packet, Sender}) ->
	State = #{
		packet => Packet,
		sender => Sender
	},
	{ok, State}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(timeout, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-ifdef(TEST).

-endif.
