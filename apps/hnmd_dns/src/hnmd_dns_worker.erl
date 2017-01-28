%%@doc DNSplice worker
%%
%% This module is responsible for processing DNS requests,
%% handing them off to a worker, and routing the replys back to
%% the client.
%%
%%@end
-module(hnmd_dns_worker).
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

-include_lib("kernel/src/inet_dns.hrl").
-define(SERVER, ?MODULE).
-define(record_to_list(Rec, Ref), lists:zip(record_info(fields, Rec),tl(tuple_to_list(Ref)))).
-define(record_to_map(Rec, Ref), maps:from_list(?record_to_list(Rec, Ref))).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%%@doc starts a worker process
%%
%% function that starts a worker process.  Should be called by the
%% hnmd_dns worker supervisor.
%%
%%@end

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%%@doc Starts the process of forwarding DNS requests
%%
%% Starts up a DNS request worker, and starts the process
%% of forwarding packets to the various backends and selecting
%% which response should be returned to the client.
%%
%%@end

-spec handle(Packet :: binary(), Sender :: {IP :: inet:ip_address(), Port :: inet:port_number()}) -> ok.

handle(Packet, Sender) ->
	{ok, _Pid} = hnmd_dns_worker_sup:start_worker(Packet, Sender),
	ok.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({Packet, Sender}) ->
	gen_server:cast(self(), setup_route),
	Timeout = application:get_env(hnmd_dns, packet_timeout, timer:seconds(1)),
	State = #{
		packet => Packet,
		sender => Sender,
		timer  => erlang:send_after(Timeout, self(), timeout)
	},
	{ok, State}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(setup_route, #{ packet := Packet } = State) ->
	{ok, Server} = application:get_env(resolver),
	{ok, Socket} = forward_packet(Server, Packet),
	Query = normalize_dns_record(Packet),
	{noreply, State#{ socket => Socket, query => Query }};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({udp, Socket, _IP, _InPortNo, ReplyPacket}, #{ sender := Sender } = State) ->
	ok = gen_udp:close(Socket),
	hnmd_dns_listener:send_reply(ReplyPacket, Sender),
	io:format("~p~n~p~n", [maps:get(query, State), normalize_dns_record(ReplyPacket)]),
	{stop, normal, State};
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

-spec forward_packet(Address :: inet:ip_address(), Packet :: binary()) ->
	{Status :: ok | {error, Reason::atom()}, Socket :: port()}.

forward_packet(Address, Packet) ->
	{ok, Socket} = gen_udp:open(0, [binary]),
	Res = gen_udp:send(Socket, Address, 53, Packet),
	{Res, Socket}.

normalize_dns_record(Packet) when is_binary(Packet) ->
	{ok, Record} = inet_dns:decode(Packet),
	normalize_dns_record(Record);
normalize_dns_record(#dns_rec{} = Record) ->
	maps:from_list([ expand_dns_rec_field(Field) || Field <- ?record_to_list(dns_rec, Record)]).

expand_dns_rec_field({header, Header}) -> {header, expand_rcode(?record_to_map(dns_header, Header))};
expand_dns_rec_field({qdlist, QDs}) ->
	{qdlist, [ all_rr_cleanup(?record_to_map(dns_query, Q)) || Q <- QDs]};
expand_dns_rec_field({arlist, OPTs}) ->
	{RRList, OPTList} = lists:partition(fun(RR)-> dns_rr == element(1, RR) end, OPTs),
	RRs    = [clean_rr(?record_to_map(dns_rr, RR)) || RR <- RRList ],
	RROpts = [all_rr_cleanup(?record_to_map(dns_rr_opt, OPT)) || OPT <- OPTList ],
	{arlist, RRs ++ RROpts};
expand_dns_rec_field({Section, RRs}) -> {Section, [ clean_rr(?record_to_map(dns_rr, RR)) || RR <- RRs]}.

clean_rr(#{ type := mx, data := {Prio, Exchange} } = RR) ->
	all_rr_cleanup(RR#{ data := #{
		priority => Prio,
		exchange => to_lower(Exchange)
	} });
clean_rr(#{ type := soa,  data := {MName,RName,Serial,Refresh,Retry,Expiry,Minimum} } = RR) ->
	all_rr_cleanup(RR#{ data := #{
		mname   => to_lower(MName),
		rname   => to_lower(RName),
		serial  => Serial,
		refresh => Refresh,
		retry   => Retry,
		expiry  => Expiry,
		minimum => Minimum
	} });
clean_rr(#{ type := Type, data := Data } = RR) when Type == a orelse Type == aaaa ->
	all_rr_cleanup(RR#{ data := to_lower(inet_parse:ntoa(Data)) });
clean_rr(#{ data := Data } = RR) when is_list(Data)  ->
	all_rr_cleanup(RR#{ data := list_to_binary(Data) });
clean_rr(#{ data := Data } = RR) when is_tuple(Data) ->
	all_rr_cleanup(RR#{ data := tuple_to_list(Data) }).

all_rr_cleanup(#{ domain := Domain } = RR) when is_list(Domain) -> all_rr_cleanup(RR#{ domain := to_lower(Domain) });
all_rr_cleanup(RR) -> maps:without([cnt, tm, bm, func], RR).

expand_rcode(#{ rcode := ?NOERROR  } = Header) -> Header#{ rcode := noerror  };
expand_rcode(#{ rcode := ?FORMERR  } = Header) -> Header#{ rcode := formerr  };
expand_rcode(#{ rcode := ?SERVFAIL } = Header) -> Header#{ rcode := servfail };
expand_rcode(#{ rcode := ?NXDOMAIN } = Header) -> Header#{ rcode := nxdomain };
expand_rcode(#{ rcode := ?NOTIMP   } = Header) -> Header#{ rcode := notimp   };
expand_rcode(#{ rcode := ?REFUSED  } = Header) -> Header#{ rcode := refused  };
expand_rcode(#{ rcode := ?NOCHANGE } = Header) -> Header#{ rcode := nochange };
expand_rcode(#{ rcode := ?BADVERS  } = Header) -> Header#{ rcode := badvers  };
expand_rcode(Header) -> Header.

to_lower(String) when is_list(String) -> list_to_binary(string:to_lower(String));
to_lower(String) when is_binary(String) ->
	<< <<(do_lower(Char)):8>> || <<Char:8>> <= String >>.

do_lower(Char) when Char >= 65 andalso Char =< 90 -> Char + 32;
do_lower(Char) -> Char.

lookup_records(Domain, Type) -≥ {ok, []}.
-ifdef(TEST).

basic_test_() ->
	{"hnmd_dns worker Tests", [
		{"Utility functions", [
			{"lowercase helper", [
				{"works on binary", ?_assertMatch(<<"az!1az">>, to_lower(<<"AZ!1az">>))},
				{"works on list", ?_assertMatch(<<"az!1az">>, to_lower("AZ!1az"))}
			]}
		]}
	]}.

-endif.
