%% @private
-module(ejabberd_rest_api).
-behaviour(application).

%% API.
-export([start/0, start/2]).
-export([stop/1]).

-define(APP, 'ejabberd_rest_api').
-define(CONF, "./priv/ejabberd_rest_api.config").

%% API.

start()->
	start(normal, ?CONF).


start(Type, Args) ->
   
    ok = ensure_started(),
    
    CfgOpts = load_config_file(Args),
    
    ClusterMaster =  proplists:get_value(cluster_master, CfgOpts),
    Host =  proplists:get_value(host, CfgOpts),
    Port =  proplists:get_value(port, CfgOpts),
    TabCopyType =   proplists:get_value(table_clone_type, CfgOpts),
    NumberAcceptors =  proplists:get_value(nb_acceptors, CfgOpts), 

    Opts = load_config(CfgOpts),
    
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
     		crypto,
    		public_key, 
    		ssl, 
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

load_config_file(ConfigFile)->
   {ok, [ConfList]} = app_config_util:load_config_file(ConfigFile),
   ConfList.

load_config(Opts)->	
    ClusterMaster =  proplists:get_value(cluster_master, Opts),
    TabCopyType =   proplists:get_value(table_clone_type, Opts),
    ClusterEthInf =  proplists:get_value(listen_interface, Opts),	
    ClusterListenIp =  proplists:get_value(listen_ip, Opts),
  	ClusterListenPort =  proplists:get_value(listen_port, Opts),
  	RefreshInterval =  proplists:get_value(refresh_interval,Opts),
  	[
  	  
      {cluster_master, ClusterMaster},
      {refresh_interval, RefreshInterval},
      {table_clone_type, TabCopyType},
      {listen_interface, ClusterEthInf},
      {listen_ip, ClusterListenIp},
      {listen_port, ClusterListenPort}
    ].	
