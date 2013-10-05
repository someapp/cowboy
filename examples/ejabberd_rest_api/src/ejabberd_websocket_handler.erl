-module(ejabberd_websocket_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

-export([
		init/3, 
		terminate/3,
		websocket_init/3,
		websocket_handle/3, 
		websocket_info/3,
		websocket_terminate/3
]).


-record(state, {
	consumer = [], data = <<"">>
}).


init({tcp, http}, Req, Opts)->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(TransportName, Req, Opts) ->
    error_logger:info_msg("~p: Transport ~p Req ~p Opt ~p~n",[?MODULE, TransportName, Req, Opts]),
	{ok, Req, #state{ consumer = []}}.

websocket_handle({text, Msg}, Req, State)->
	error_logger:error_msg("~p: Received Req:~p~n",
		[?MODULE, Req]),
	Payload = <<>>,	
	{reply, Payload, Req, State, hibernate};	
	
websocket_handle(Message, Req, State)->
	error_logger:error_msg("~p: Received Unknown Req:~p~n",
		[?MODULE, Req]),	
	{ok, Req, State}.

websocket_info({rabbit, Msg}, Req, State)->
	error_logger:info_msg("~p: Received Req:~p~n",[?MODULE, Req]),	
	{ok, {text, Msg},Req, State};

websocket_info({timeout, _Ref, Msg}, Req, State) ->
	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{reply, {text, Msg}, Req, State};

websocket_info(Info, Req, State)->
	error_logger:error_msg("~p: Received Req:~p~n",[?MODULE, Req]),	
	{ok, Req, State, hibernate}.	
	
websocket_terminate(Reason, Req, State)->
	error_logger:info_msg("~p: Terminate :~p~n",
		[?MODULE, 'spark_rabbit_consumer']),
%	spark_rabbit_consumer:stop(Reason),	
	ok.

deserialize(Msg) ->
	Ret = <<"test">>,
	Ret.
	
terminate(Reason, Req, State)->
	ok.
	
encode_response(Encoder, Jid, OnlineNow) ->
	Encoder:ensure_binary(Jid, OnlineNow).
	
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

get_response_body({error, Unknown}, Req)->
	Unknown1 = erlang:atom_to_binary(Unknown),
	get_response_body(500, Unknown1, Req).
				 	 
get_response_body(Code, Error, Req)->
	cowboy_req:reply(500, get_response_meta(),
				 	 jsx:encode(Error),
				 	 Req).	
	
