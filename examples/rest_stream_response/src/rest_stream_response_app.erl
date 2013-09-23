%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(rest_stream_response_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
-define(APP, ?MODULE).
-define(CONF, "./priv/ejabberd_rest_api.config").
-define(STARTYPE, permanent).



start(_Type, Args) ->
	Table = ets:new(stream_tab, []),
	generate_rows(Table, 1000),

	error_logger:info_msg("Starting ~p with arguments: ~p~n",
				 [?APP, lists:flatten(Args)]),   
    ok = ejabberd_rest_api_config:ensure_started(),
    
    CfgOpts = ejabberd_rest_api_config:load_config_file(Args),
    
  % ClusterMaster =  proplists:get_value(cluster_master, CfgOpts),
    Host =  proplists:get_value(host, CfgOpts),
    Port =  proplists:get_value(port, CfgOpts),
  %  TabCopyType =   proplists:get_value(table_clone_type, CfgOpts),
  %  EjabTabs = Tables = proplists:get_value(ejab_table, CfgOpts),
    NumberAcceptors =  proplists:get_value(nb_acceptors, CfgOpts), 
%    StartType = proplists:get_value(start_type, CfgOpts),
%    Opts = ejabberd_rest_api_config:load_config(CfgOpts),
%    Opts0 =lists:concat([Opts,[{start_type, StartType}]]),
		
%	Dispatch = cowboy_router:compile([
%		{'_', [
%			{"/[:v1]", [{v1, int}], toppage_handler, Table}
%		]}
%	]),
	Dispatch = url_route_map:route_map2(Host, [], [Host], Table),
	{ok, _} = cowboy:start_http(http, NumberAcceptors , [{port,  Port}], [
		{env, [{dispatch, Dispatch}]},
		{onresquest, fun spark_rest_error_handler:onrequest_hook/3},
		{onresponse, fun spark_rest_error_handler:onresponse_hook/4}
	]),
	rest_stream_response_sup:start_link().

stop(_State) ->
	ok.

generate_rows(_Table, 0) ->
	ok;
generate_rows(Table, N) ->
	ets:insert(Table, {key(), val(), val()}),
	generate_rows(Table, N - 1).

key() -> key(10).
key(N) -> key(<< (random:uniform(26) - 1) >>, N - 1).
key(Acc, 0) -> binary_part(base64:encode(Acc), 0, 8);
key(Acc, N) -> key(<< Acc/binary, (random:uniform(26) - 1) >>, N - 1).
val() -> random:uniform(50).

