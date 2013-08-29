%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(ejabberd_rest_api_sup).
-behaviour(supervisor).

%% API.
-export([start_link/1]).

%% supervisor.
-export([init/1]).
-define (SERVER, ?MODULE).
-define(APP, 'ejabberd_rest_api').

-define(CHILD(I, Type, Opt), {I, {I, start_link, Opt}, permanent, 5000, Type, [I]}).

%% API.

-spec start_link(list()) -> {ok, pid()}.
start_link(Args) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%% supervisor.

init(Args) ->
	ClusterMaster = proplists:get_value(cluster_master, Args),
	RefreshInterval = proplists:get_value(refresh_interval, Args),
	TableCloneType = proplists:get_value(table_clone_type, Args),
	Environment = proplists:get_value(environment, Args),
	Opt1 = [{cluster_master, ClusterMaster},
    		{refresh_interval, RefreshInterval},
    		{environment, Environment}
           ],
    Opt2 = [{cluster_master, ClusterMaster},
  			{table_clone_type, TableCloneType}
    	   ],
    			
	Children = lists:flatten([
    	?CHILD(user_presence_srv, worker, Opt1),
    	?CHILD(user_presence_db, worker, Opt2)
    ]),
	{ok, {{one_for_one, 10, 10}, Children}}.
