%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(ejabberd_rest_api_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, Args) ->
    Host = proplist:get_value(host, Args),
    Port = proplist:get_value(port, Args),
    TabCopyType =  proplist:get_val(table_copy_type, Opts),
    NumAcceptors = proplist:get_value(nbacceptors, Args),
    ClusterMaster = proplist:get_value(cluster_master, Args),
    BackoffStrategy = proplist:get_value(backoff_strategy, Args),
    Opts = [{host, Host},
    		{table_copy_type, TabCopyType},
    		{cluster_master, ClusterMaster},
    	   ]
	Dispatch = cowboy_router:compile(url_route_map:route_map(Host,Opts)),
	{ok, _} = cowboy:start_http(http, NumAcceptors , 
		[{port, Port}], 
		[{env, [{dispatch, Dispatch}]}
	]).
%	ejabberd_rest_api_sup:start_link(Opts).

stop(_State) ->
	ok.


