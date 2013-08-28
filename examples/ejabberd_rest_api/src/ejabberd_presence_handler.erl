-module(ejabberd_presence_handler).
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
	cluster_head :: atom(),
	table_copy_type :: atom(),
	
}).

init({tcp, http}, Req, Opts)->
	ClusterHead = proplist:get_valy(cluster_head, Opts),
	TabCopyType =  proplist:get_val(table_copy_type, Opts),
	ensure_mnesia_available(ClusterHead),
	{upgrade, protocol, cowboy_rest, Req, 
	 #state{
		cluster_head = ClusterHead,	
		table_copy_type = TabCopyType
	}}.

rest_init(Req, State)->

	{ok, Req, State}.

rest_terminate(Req, State)->

	ok.
	
allowed_methods(Req, State)->	
	{[<<"GET">>], Req, State}.

content_types_provides(Req, State) ->
	{[ 
		{{<<"application">>, <<"json">>, []}, is_user_online}
	], Req, State}.	
	
ensure_mnesia_available(ClusterHead) when is_atom(ClusterHead) ->
	%% some sanity check
	{ok, online}.
	
	

