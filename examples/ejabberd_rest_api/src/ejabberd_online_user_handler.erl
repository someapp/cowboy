-module(ejabberd_online_user_handler).
%-behaviour(cowboy_rest_handler).

-export([
		init/3, 
		rest_init/2, 
		rest_terminate/2
]).

-export([
		allowed_methods/2,
		known_methods/2,
		content_types_provided/2
		]).

-export([get_resource/2]).		
-include_lib("ejab_api.hrl").
-include_lib("online_user.hrl").

-record(state, {
	cluster_head :: atom(),
	table_copy_type :: atom(),
	data_model :: atom(),
	method_supported :: list(),
	data :: binary()
}).

-define(DATAMODEL, 'online_user').



init({tcp, http}, Req, Opts)->
	ClusterHead = proplists:get_value(cluster_head, Opts),
	TabCopyType =  proplists:get_value(table_copy_type, Opts),
	DataModel = ?DATAMODEL,
	{upgrade, protocol, cowboy_rest, Req, 
	 #state{
		cluster_head = ClusterHead,	
		table_copy_type = TabCopyType,
		data_model = DataModel,
		method_supported = [<<"GET">>, <<"HEAD">>, <<"OPTIONS">>],
		data = <<"">>
	}}.

rest_init(Req, State)->
	{ok, Req, State}.

rest_terminate(Req, State)->
	ok.
	
terminate(Reason, Req, State)->
	ok.
	
get_resource(Req, State)->
	{Jid0, Req1} = cowboy_req:qs_val(<<"jid">>, Req),
	
	Body = case user_presence_srv:list_online(Jid0) of
		{ok, online} -> 
						 DataModel = State#state.data_model,
						 encode_response(DataModel, Jid0, <<"online">>);
						 
		{ok, offline} -> 
						 DataModel = State#state.data_model,
						 encode_response(DataModel, Jid0, <<"offline">>);
						 
		{error, Reason} -> fail(Req1, 
								State#state{ data = <<"offline">>});
		E -> fail(Req, {error, E})
	end,
	{Body, Req, State}.

get_userId(Jid)->
 	{U, S} = spark_jid:raw_split_jid(Jid),
	{U, S}.
	%app_util:to_integer(U).

encode_response(Encoder, Jid, OnlineNow) ->
	Encoder:ensure_binary(Jid, OnlineNow).


options(Req, State)->
	Allowed = erlang:list_to_binary(State#state.method_supported),
    cowboy_req:set_resp_header(<<"allow">>, Allowed, Req).
	
	
allowed_methods(Req, State)->	
	{[<<"GET">>], Req, State}.
	
known_methods(Req, State)->
 	{State#state.method_supported, Req, State}.

content_types_provided(Req, State) ->
	{[ 
		{{<<"application">>, <<"json">>, []}, get_resource}
	], Req, State}.	
	
	
fail(Req, State = #state{data = Error}) when is_atom(Error)->
	fail(Req, Error, State);
fail(Req, State = #state{data = Error}) when is_binary(Error)->
	fail(Req, Error, State).
		 	
fail(Req, Error, State) ->
	{ok, Req1} = get_response_body(Error, Req),
	{halt, Req1, State}.
	
get_response_meta()->
	[
		{<<"content-type">>, <<"application/json; charset=UTF-8">>},
		{<<"cache-control">>, <<"no-store">>},
		{<<"pragma">>, <<"no-cache">>}
	].

get_response_body({error, badarg}, Req)->
	get_response_body(400, <<"badarg">> , Req);
	
get_response_body({error, no_exists}, Req)->
	get_response_body(404, <<"no_exists">> , Req);
	
get_response_body({error, already_exists}, Req)->
	get_response_body(400, <<"already_exists">> , Req);

get_response_body({error, mnesia_down}, Req)->
	get_response_body(503, <<"mnesia_down">> , Req);
	
get_response_body({error, node_not_running}, Req)->
	get_response_body(503, <<"node_not_running">> , Req);

get_response_body({error, no_transaction}, Req)->
	get_response_body(400, <<"no_transaction">> , Req);
	
get_response_body({error, combine_error}, Req)->
	get_response_body(400, <<"combine_error">> , Req);				 	 

get_response_body({error, illegal}, Req)->
	get_response_body(405, <<"illegal">> , Req);
			
get_response_body({error, active}, Req)->
	get_response_body(400, <<"active">> , Req);		

get_response_body({error, not_a_db_node}, Req)->
	get_response_body(405, <<"not_a_db_node">> , Req);		

get_response_body({error, system_limit}, Req)->
	get_response_body(431, <<"system_limit">> , Req);
		
get_response_body({error, truncated_binary_file}, Req)->
	get_response_body(500, <<"truncated_binary_file">> , Req);	
	
get_response_body({error, bad_index}, Req)->
	get_response_body(400, <<"bad_index">> , Req);
	
get_response_body({error, index_exists} , Req)->
	get_response_body(400, <<"index_exists">> , Req);				 	 			 	 
get_response_body({error, bad_type}, Req)->
	get_response_body(400, <<"bad_type">> , Req);

get_response_body({error, Unknown}, Req) when is_atom(Unknown) ->
	Unknown1 = erlang:atom_to_binary(Unknown),
	get_response_body(500, Unknown1, Req);
	
get_response_body({error, _}, Req)  ->
	get_response_body(500, 
			erlang:atom_to_binary(unknown_error), Req).
			
get_response_body(Code, {json_encoded, Body}, Req)->
	{ok, Req1} = cowboy_req:reply(Code, get_response_meta(),
				 	 Body, Req),
	cowboy_req:body(Req1);	
				 	 
get_response_body(Code, Body, Req)->
	{ok, Req1} = cowboy_req:reply(Code, get_response_meta(),
				 	 jsx:encode(Body), Req),
	cowboy_req:body(Req1).					 	 

get_response_body2(_Code, {json_encoded, Body}, _Req)-> Body;
get_response_body2(_Code, Body, _Req)-> jsx:encode(Body).
