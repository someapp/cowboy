%% @private
-module(ejabberd_rest_api).
-behaviour(application).

%% API.
-export([start/0, start/2]).
-export([stop/1]).

-define(APP, ?MODULE).
-define(HANDLER, ?MODULE).

-define(?FORWARD1(M, F), F(X) -> M:F(X)).
-define(?FORWARD2(M, F), F(X,Y) -> M:F(X,Y)).
-define(?FORWARD3(M, F), F(X,Y,Z) -> M:F(X,Y,Z)).



%% API.

start()->
	Args = application:get_all_env(?APP),
	start(_Type, Args).
    

start(_Type, Args) ->
    ok = sensure_started(),
    ClusterMaster = proplist:get_value(cluster_master, Args),
    Host = proplist:get_value(host, Args),
    Port = proplist:get_value(port, Args),
    TabCopyType =  proplist:get_val(table_copy_type, Opts),
    NumberAcceptors = proplist:get_value(nb_acceptors, Args), 
    ClusterEthInf = proplist:get_value(listen_interface, Args),	
    ClusterListenIp = proplist:get_value(listen_ip, Args),
  	ClusterListenPort = proplist:get_value(listen_port, Args),
    Opts = [
    		{cluster_master, ClusterMaster},
    		{table_copy_type, TabCopyType},
    		{listen_interface, ClusterEthInf},
    		{listen_ip, ClusterListenIp},
    		{listen_port, ClusterListenPort},
    ],
    
	Dispatch = cowboy_router:compile(url_route_map:route_map(Host, Opts)),
	{ok, Ret} = cowboy:start_http(http, NumberAcceptors, 
		[{port, Port}], 
		[{env, [{dispatch, Dispatch}]},
		{onrequest, fun ?HANDLER:debug/1}
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

debug_hook(Req)->
	%?FORWARD2(info, "Incoming Request: ~p~n", [Req]),
	?FORWARD2(display, Req). 
	Req.
	
debug_hook(Code, Headers, Req)->
	?FORWARD3(info, "On response: ~p~n", [Req]),
	Req.	

?FORWARD1(tty, error_logger:tty). 
?FORWARD2(display, erlang:display). 
?FORWARD2(warn, error_logger:warning_msg). 
?FORWARD2(error, error_logger:error_msg). 
?FORWARD2(info, error_logger:info_msg). 
	
