%% @private
-module(ejabberd_rest_api).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

-define(APP, 'ejabberd_rest_api').
-define(CONF, "./priv/ejabberd_rest_api.config").
-define(STARTYPE, permanent).

%% API.


start(normal, Args) ->
	error_logger:info_msg("Starting ~p with arguments: ~p~n",
				 [?APP, lists:flatten(Args)]),   
    ok = ensure_started(),
    
    CfgOpts = load_config_file(Args),
    
  % ClusterMaster =  proplists:get_value(cluster_master, CfgOpts),
    Host =  proplists:get_value(host, CfgOpts),
    Port =  proplists:get_value(port, CfgOpts),
  %  TabCopyType =   proplists:get_value(table_clone_type, CfgOpts),
  %  EjabTabs = Tables = proplists:get_value(ejab_table, CfgOpts),
    NumberAcceptors =  proplists:get_value(nb_acceptors, CfgOpts), 
    StartType = proplists:get_value(start_type, CfgOpts),
    Opts = load_config(CfgOpts),
    Opts0 = lists:concat([Opts,[{start_type, StartType}]]),

%   	Dispatch = url_route_map:route_map(Host, [], [Host]),
	Dispatch = get_dispatch(Host),
	
   	{ok, R} = cowboy:start_http(http, NumberAcceptors, 
   		[{port, Port}], 
   		[{compress, true},
   		 {env, [{dispatch, Dispatch}]},		 
   		 {onrequest, fun error_hook_responder:onrequest_hook/1}
		%,{onresponse, fun error_hook_responder:onresponse_hook/4}
		,{onresponse, fun error_hook/4}
    ]),
	Ret = ejabberd_rest_api_sup:start_link(Opts0),	
	error_logger:info_msg("Start ejabberd api status: ~p~n",
				 [Ret]),
    %{ok, Ret};
	Ret;
	
start(A,B)->
	{error, {A,B, badarg}}.

stop(_State) ->
	ok.

get_dispatch(Host)->
	url_route_map:route_map(Host, [], 'ejabberd_online_data', []).

ensure_started()->
 	%reloader:start(),
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
    EjabTabs = get_config_value(ejab_table, Opts),
    StartType = get_config_value(start_type, Opts),
    ClusterEthInf =  get_config_value(listen_interface, Opts),	
    ClusterListenIp =  get_config_value(listen_ip, Opts),
  	ClusterListenPort =  get_config_value(listen_port, Opts),
  	RefreshInterval =  get_config_value(refresh_interval,Opts),
  	Options = [
	  {environment, Environment},
      {cluster_master, ClusterMaster},
      {refresh_interval, RefreshInterval},
      {table_clone_type, TabCopyType},

   	
   	  {ejab_table, EjabTabs},
 	  {start_type, StartType},      
      
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


error_hook(404, Headers, <<>>, Req) ->
	{Path, Req2} = cowboy_req:path(Req),
	Body = ["404 Not Found: \"", Path,
		"\" is not the path you are looking for.\n"],
	Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
		{<<"content-length">>, integer_to_list(iolist_size(Body))}),
	{ok, Req3} = cowboy_req:reply(404, Headers2, Body, Req2),
	Req3;
error_hook(Code, Headers, <<>>, Req) when is_integer(Code), Code >= 400 ->
	Body = ["HTTP Error ", integer_to_list(Code), $\n],
	Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
		{<<"content-length">>, integer_to_list(iolist_size(Body))}),
	{ok, Req2} = cowboy_req:reply(Code, Headers2, Body, Req),
	Req2;
error_hook(_Code, _Headers, _Body, Req) ->
	Req.



