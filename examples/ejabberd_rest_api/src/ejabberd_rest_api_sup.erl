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

-define(CHILD(I, Type, StartType, Opt), {I, {I, start_link, Opt}, StartType, 5000, Type, [I]}).

%% API.

-spec start_link(list()) -> {ok, pid()}.
start_link(Args) ->
    error_logger:info_msg("Start link ~p ~p~n",
    			[?SERVER, lists:flatten(Args)]),
	supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%% supervisor.
-spec init(list()) -> {ok, pid()}.
init(Args) ->
	ClusterMaster = proplists:get_value(cluster_master, Args),
	RefreshInterval = proplists:get_value(refresh_interval, Args),
	TableCloneType = proplists:get_value(table_clone_type, Args),
	Environment = proplists:get_value(environment, Args),
	StartType =  proplists:get_value(start_type, Args),
	Opt1 = [{cluster_master, ClusterMaster},
    		{refresh_interval, RefreshInterval},
    		{environment, Environment}
           ],
    Opt2 = [{cluster_master, ClusterMaster},
  			{table_clone_type, TableCloneType}
    	   ],
    			
	Children = lists:flatten([
	%	?CHILD(mychild_server, worker, StartType, [Opt1])
    	?CHILD(user_presence_srv, worker, StartType, [Opt1]),
    	?CHILD(user_presence_db, worker, StartType, [Opt2])
    ]),
	error_logger:info_msg("Initiating sub applications: ~p ~p~n",
			[user_presence_srv, user_presence_db]),
	error_logger:info_msg("~p: with Option: ~p~n",
			[user_presence_srv, lists:flatten(Opt1)]),		
	error_logger:info_msg("~p: with Option: ~p~n",
			[user_presence_db, lists:flatten(Opt2)]),
	{ok, {{one_for_one, 10, 10}, Children}}.
