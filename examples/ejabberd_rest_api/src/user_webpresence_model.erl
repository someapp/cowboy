-module(user_webpresence_model).

-include_lib("jsonrec.hrl").
-include_lib("user_webpresence.hrl").

-export([encode/1, decode/1]).
-export_records([user_webpresence]).
encode(#user_webpresence{} = Rec) ->
	?encode_gen(#user_webpresence{}, Rec).

decode(Payload) ->
	?encode_gen(#user_webpresence{}, Payload).
