-module(hnmd_util_stats).

-export([
	new/1,
	new/2,
	get/1,
	add/2,
	update/3
]).

-record(hnmd_moving_percentile, {percentile, pval, last}).

new(Percentile) -> new(Percentile, []).

new(Percentile, Opts) -> #hnmd_moving_percentile{ percentile=Percentile }.
