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

new(Percentile, Opts) when Percentile > 0 andalso Percentile =< 1 ->
	#hnmd_moving_percentile{ percentile=Percentile, last=undefined, pval=proplists:get_value(pval, Opts, 0.005) }.

get(#hnmd_moving_percentile{ last=Value }) -> Value.

add(Value, Stat) -> update_stat(Value, Stat).

update(Value, Stat) ->
	NewStat = #hnmd_moving_percentile{ last=NewValue } = update_stat(Value, Stat),
	{NewValue, NewStat}.

update_stat(Value, #hnmd_moving_percentile{ last=undefined } = Stat) -> Stat#hnmd_moving_percentile{ last=Value };
update_stat(Value, #hnmd_moving_percentile{ last=Last } = Stat) when Value == Last -> Stat;
update_stat(Value, #hnmd_moving_percentile{ last=Last, percentile=Perc, pval=Pval } = Stat) when Value <  Last ->
	Stat#hnmd_moving_percentile{ last= (Last - (Pval/Perc)) };
update_stat(Value, #hnmd_moving_percentile{ last=Last, percentile=Perc, pval=Pval } = Stat) when Value >  Last ->
	Stat#hnmd_moving_percentile{ last= (Last + (Pval/(1-Perc))) }.
