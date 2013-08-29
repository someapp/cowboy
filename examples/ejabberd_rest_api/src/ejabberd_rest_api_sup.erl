%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(ejabberd_rest_api_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).
-define (SERVER, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link(Args) ->
	Args = application:get_all_env(?APP),
	supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%% supervisor.

init(Args) ->
	ClusterMaster = proplists:get_value(cluster_master, Args),
	RefreshInterval = proplists:get_value(refresh_interval, Args),
	TableCloneType = proplists:get_value(table_clone_type, Args),
	Opts = [ClusterMaster, ],
	Children = lists:flatten([
    	?CHILD(user_presence_srv, worker, 
    			[{cluster_master, ClusterMaster},
    			 {refresh_interval, RefreshInterval}	
    			]),
    	?CHILD(user_presence_db, worker, 
    			[{cluster_master, ClusterMaster},
    			 {table_clone_type, TableCloneType}
    			])
    ]),
	{ok, {{one_for_one, 10, 10}, Children}}.
