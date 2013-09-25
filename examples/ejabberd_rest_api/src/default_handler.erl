-module(default_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

-spec init(tuple(), atom(), list()) -> {ok, term(), atom()}.
init({_, http}, Req, Opts) ->
	error_logger:info_msg("Unknown resource access Request: ~p Opts ~p~n ",[Req, Opts]),
	{ok, Req, undefined}.

-spec handle(atom, any())-> {ok, atom(), any()}.
handle(Req, State)->
    {Method, Req1} = cowboy_req:method(Req),
	{ok, Req2} = echo(Method, Req1),				
	{ok, Req2, State}.

-spec terminate(atom, any(), any()) -> ok.
terminate(Reason,_,_)->
	error_logger:info_msg("Default handler terminated with reason: ~p~n ",[Reason]),
	ok.

echo(_, Req) ->
	%% Method not allowed.
	cowboy_req:reply(403,[],<<"socialstream is running">>, Req).
