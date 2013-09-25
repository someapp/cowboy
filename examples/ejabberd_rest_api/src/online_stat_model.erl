-module(online_stat_model).
-include_lib("jsonrec.hrl").
-include_lib("online_stat.hrl").

-export([encode/1, decode/1]).
encode(#online_stat{} = Rec) ->
	?encode_gen(#online_stat{}, Rec).

decode(Payload) ->
	?encode_gen(#online_stat{}, Payload).
		
