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
	supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%% supervisor.

init(Args) ->
	
	Children = lists:flatten([
    	?CHILD(user_presence_srv, worker, Opts),
    	?CHILD(user_presence_db, worker, Opts)
    ]),
	{ok, {{one_for_one, 10, 10}, Children}}.
