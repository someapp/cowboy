-module(online_user).
-include_lib("online_user_stat.hrl").
-behaviour(json_rec_model).

-compile({parse_transform, exprecs}).
-export([new/1, 
	 	 rec/1,
	  	 ensure_binary/1]).
-spec new( bitstring() )-> #online_user_stat{} | undefined.
new(<<"online_user_stat">>)->
   '#new-online_user_stat'();
new(_)-> undefined.

rec(#online_user_stat{} =Value) ->  Value;
rec(_)-> undefined.

-spec ensure_binary(atom() | any()) -> binary().
ensure_binary(#online_user_stat{} = Value) ->
    Json = json_rec:to_json(Value, online_user),
    lists:flatten(mochijson2:encode(Json));
ensure_binary(Val) -> app_util:ensure_binary(Val).
