%% @private
-module(ejabberd_rest_api).
-behaviour(application).

%% API.
-export([start/0, start/2]).
-export([stop/1]).

-define(APP, ?MODULE).


%% API.

start()->
	Args = application:get_all_env(?APP),
	start(normal, Args).
    

start(Type, Args) ->
    ok = ensure_started(),
    ClusterMaster = proplist:get_value(cluster_master, Args),
    Host = proplist:get_value(host, Args),
    Port = proplist:get_value(port, Args),
    TabCopyType =  proplist:get_val(table_copy_type, Args),
    NumberAcceptors = proplist:get_value(nb_acceptors, Args), 
    ClusterEthInf = proplist:get_value(listen_interface, Args),	
    ClusterListenIp = proplist:get_value(listen_ip, Args),
  	ClusterListenPort = proplist:get_value(listen_port, Args),
  	RefreshInterval = proplist:get_value(refresh_interval, Args),
    Opts = [
    		{cluster_master, ClusterMaster},
    		{refresh_interval, RefreshInterval},
    		{table_copy_type, TabCopyType},
    		{listen_interface, ClusterEthInf},
    		{listen_ip, ClusterListenIp},
    		{listen_port, ClusterListenPort}
    ],
    
	Dispatch = cowboy_router:compile(url_route_map:route_map(Host, Opts)),
	{ok, Ret} = cowboy:start_http(http, NumberAcceptors, 
		[{port, Port}], 
		[{env, [{dispatch, Dispatch}]},
		{onrequest, fun cowboy_debug:onrequest_hook/1},
		{onresponse, fun cowboy_debug:onresponse_hook/3}
	]),
	ejabberd_rest_api_sup:start_link(Opts).

stop(_State) ->
	ok.

ensure_started()->
 	reloader:start(),
    Apps = [ 
   
    		ssl, 
    		public_keys, 
    		crypto, 
    		inets,
    		syntax_tools,
    		compiler,
   % 		goldrush,
    		mnesia, 
    		ranch,
    		parse_trans,
    		json_rec,
    		cowboy
    		],
    app_util:start_apps(Apps),
    ok.	



	
