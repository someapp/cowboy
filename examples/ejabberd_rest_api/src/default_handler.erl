-module(default_handler).
%-behaviour(cowboy_rest_handler).

-export([
		init/3,
		rest_init/2, 
		rest_terminate/2
]).

-export([
		allowed_methods/2,
		content_types_provided/2,
		content_types_accepted/2
		]).

-record(state, {
}).

init({tcp, http}, Req, Opts)->
	{upgrade, protocol, cowboy_rest, Req, 
	 #state{}}.

rest_init(Req, State)->
	
	{ok, Req, State}.

rest_terminate(Req, State)->

	ok.
	
allowed_methods(Req, State)->	
	{[<<"GET">>, 
	  <<"PUT">>, 
	  <<"HEAD">>, 
	  <<"POST">>, 
	  <<"PATCH">>, 
	  <<"DELETE">>,
	  <<"OPTIONS">>], 
	  Req, State}.

content_types_accepted(Req, State)->
	{[
	 
	], Req, State}.	

content_types_provides(Req, State) ->
	{[ 
		{{<<"application">>, <<"json">>, []}, default_action}
	], Req, State}.	
	
	
	

