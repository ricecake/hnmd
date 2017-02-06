-module(hnmd_dhcp).

-export([
	parse_dhcp/1,
	make_dhcp/1
]).

parse_dhcp(_Packet) -> {ok, #{}}.

make_dhcp(_Data) -> {ok, <<>>}.
