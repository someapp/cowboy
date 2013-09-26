-module(online_user_set_model).
-include_lib("online_user_set.hrl").
-include_lib("jsonrec.hrl").

-export([encode/1, decode/1]).
encode(#online_user_set{} = Rec) ->
	error_logger:info_msg("~p:encode ~n", [?MODULE]),
	?encode_gen(#online_user_set{}, Rec).

decode(Payload) ->
	?encode_gen(#online_user_set{}, Payload).
