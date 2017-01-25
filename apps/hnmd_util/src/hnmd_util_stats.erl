-module(hnmd_util_stats).

-export([
	new/1,
	new/2,
	get/1,
	add/2,
	update/2
]).

-record(hnmd_moving_percentile, {percentile, pval, last}).

new(Percentile) -> new(Percentile, []).

new(Percentile, _Opts) -> #hnmd_moving_percentile{ percentile=Percentile }.

get(_Stat) -> ok.

add(_Value, Stat) -> Stat.

update(Value, Stat) -> {Value, Stat}.
