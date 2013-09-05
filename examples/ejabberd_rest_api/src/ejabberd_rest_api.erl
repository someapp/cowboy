%% @private
-module(ejabberd_rest_api).
-behaviour(application).

%% API.
-export([start/0, start/2]).
-export([stop/1]).

-define(APP, 'ejabberd_rest_api').
-define(CONF, "./priv/ejabberd_rest_api.config").
-define(STARTYPE, permanent).

%% API.

start()->
	start(?STARTYPE, ?CONF).


start(Type, Args) ->
   
    ok = ensure_started(),
    
    CfgOpts = load_config_file(Args),
    
    ClusterMaster =  proplists:get_value(cluster_master, CfgOpts),
    Host =  proplists:get_value(host, CfgOpts),
    Port =  proplists:get_value(port, CfgOpts),
    TabCopyType =   proplists:get_value(table_clone_type, CfgOpts),
    NumberAcceptors =  proplists:get_value(nb_acceptors, CfgOpts), 

    Opts = load_config(CfgOpts),
    Opts0 = lists:concat([Opts,[{start_type, Type}]]),

    R = ejabberd_rest_api_sup:start_link(Opts0),
   	Dispatch = cowboy_router:compile(url_route_map:route_map(Host, [])),
	
   	{ok, Ret} = cowboy:start_http(http, NumberAcceptors, 
   		[{port, Port}], 
   		[{compress, true},
   		 {env, [{dispatch, Dispatch}]},		 
   		 {onrequest, fun error_hook_responder:onrequest_hook/1},
		 {onresponse, fun error_hook_responder:onresponse_hook/3}
    ]),
	
	%R = ejabberd_rest_api_sup:start_link(Opts),
	error_logger:info_msg("Start ejabberd api status: ~p~n",
				 [R]),
	Ret.

stop(_State) ->
	ok.

ensure_started()->
 	reloader:start(),
    Apps = [ 
     		crypto,
    		public_key, 
    		ssl, 
    		inets,
    		sasl,
    		syntax_tools,
    		compiler,
   % 		goldrush,
    		mnesia, 
    		parse_trans,
    		json_rec,
    		ranch,
    		cowlib,
    		cowboy
    		],
    app_util:start_apps(Apps),
    ok.	

load_config_file(ConfigFile)->
   {ok, [ConfList]} = app_config_util:load_config_file(ConfigFile),
   ConfList.

load_config(Opts)->
	%Opts = [],	
    Environment = get_config_value(environment, Opts),
    ClusterMaster = get_config_value(cluster_master, Opts),
    TabCopyType = get_config_value(table_clone_type, Opts),
    ClusterEthInf =  get_config_value(listen_interface, Opts),	
    ClusterListenIp =  get_config_value(listen_ip, Opts),
  	ClusterListenPort =  get_config_value(listen_port, Opts),
  	RefreshInterval =  get_config_value(refresh_interval,Opts),
  	Options = [
	  {environment, Environment},
      {cluster_master, ClusterMaster},
      {refresh_interval, RefreshInterval},
      {table_clone_type, TabCopyType},
      {listen_interface, ClusterEthInf},
      {listen_ip, ClusterListenIp},
      {listen_port, ClusterListenPort}
    ],
    lists:flatten(Options).

get_config_value(Key,[]) when is_atom(Key) ->
	case application:get_env(Key) of
		{ok, Val} -> Val;
		undefined -> undefined
	end;
get_config_value(Key, Opt) when is_atom(Key)->
	proplists:get_value(Key, Opt).
