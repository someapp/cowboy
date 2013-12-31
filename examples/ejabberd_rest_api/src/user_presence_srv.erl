-module(user_presence_srv).
-behaviour(gen_server).

-export([list_online/1
	 ,log_user_off/2
	 ,log_user_off/3
	 ,list_online_count/0
	 ,list_online_count/1
	 ,list_online_count/2
	 ,list_all_online/0
 	 ,list_all_online/1
	 ,list_all_online/2
     	 ,generate_token/0]).

-export([sync_session_from_cluster/0]).

-export([start_link/1]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2, 
	 terminate/2, 
	 code_change/3]).

-include_lib("ejab_api.hrl").
-include_lib("user_webpresence.hrl").

-define(SERVER, ?MODULE).
-define(COPY_TYPE, disc_copies).

-define(ConfPath,"conf").
-define(ConfFile, "spark_user_presence.config").

-record(state,{
        cluster_master,
        vhost,
	refresh_interval = -1, 
	environment = <<"">>,
	last_check
}).


-type state() :: #state{}.

start_link(Args)->
  error_logger:info_msg("Start_link ~p with Args ~p~n",
  			 [?SERVER, Args]),
  R = gen_server:start_link({local, ?SERVER}, ?MODULE, Args ,[]),
  error_logger:info_msg("~p started with pid ~p~n",[?SERVER, R]),
  R.   

init(Args)->
  %process_flag(trap_exit,true),
  Start = app_util:os_now(),
  error_logger:info_msg("Initiating ~p with config ~p ~n",
  			 [?SERVER, Args]),
  Interval = proplists:get_value(refresh_interval, Args), 
  ClusterMaster = proplists:get_value(cluster_master, Args),
  Environment = proplists:get_value(environment, Args),
  VHost = proplists:get_value(vhost, Args),

  End = app_util:os_now(),
  error_logger:info_msg("Done Initiation ~p Start ~p End ~p", 
  			 [?SERVER, Start, End]),
  {ok, #state{cluster_master = ClusterMaster, 
  	   vhost = VHost,
       refresh_interval = Interval,
       environment = Environment,
       last_check=End}}.

start()->
   gen_server:call(?SERVER, start).

stop()->
 	gen_server:call(?SERVER, stop).

sync_session_from_cluster()->
  gen_server:call(?SERVER, sync_session_from_cluster).

log_user_off(UserId, Server)->
  error_logger:info_msg("~p:log user off User: ~p Server:~p ~n", [?MODULE, UserId, Server]) , 
  gen_server:call(?SERVER,{log_user_off, UserId, Server}).

log_user_off(UserId, Server, Resource)->
  error_logger:info_msg("~p:log user off User: ~p Server: ~p Resource:~p ~n",
	 [?MODULE, UserId, Server, Resource]),  
  gen_server:call(?SERVER,{log_user_off, UserId, Server, Resource}).
  
list_online(UserId) ->
  error_logger:info_msg("~p:list_online: ~p~n", [?MODULE, UserId]),
  gen_server:call(?SERVER,{list_online, UserId}).

list_all_online() ->
  list_all_online(call, undefined).

list_all_online(Since) ->
  list_all_online(call, Since).

list_all_online(Type, Since) when is_atom(Type) ->
  error_logger:info_msg("~p:list_all_online ~n", [?MODULE]),
  gen_server:Type(?SERVER,{list_all_count, Since}).

list_online_count()->
  error_logger:info_msg("~p:list_online_count ~n", [?MODULE]),
  list_online_count(call, undefined).

list_online_count(Since)->
  error_logger:info_msg("~p:list_online_count ~n", [?MODULE]),
  list_online_count(call, Since).

list_online_count(Type, Since) when is_atom(Type)->
  error_logger:info_msg("~p:list_online_count/2 ~n", [?MODULE]),
  gen_server:Type(?SERVER, {list_online_count, Since}).

handle_call({list_online, UserId}, _From, State)->
  error_logger:info_msg("~p:handle_call list_online ~p~n",
  		[?MODULE, UserId]),
  {LUser, LServer} = spark_jid:raw_split_jid(UserId),
  error_logger:info_msg("~p:list_online: Query against: ~p@~p~n",
  		[?MODULE, LUser, LServer]),
  Online = get_all_sessions(LUser, LServer),
  error_logger:info_msg("~p:list_online json response: ~p~n",
  		[?MODULE, Online]),
  {reply, Online, State};

handle_call({list_all_count, Since}, _From, State)->
  VHost = State#state.vhost,
  OnlineUsers =  list_online_users(),
  error_logger:info_msg("~p:list_all_count: ~p~n",
  			[?MODULE, OnlineUsers]),
  {reply, {ok, OnlineUsers}, State};

handle_call({list_online_count, Since}, _From, State)->
  error_logger:info_msg("~p:handle_call list_online count since:~p~n",
  	[?SERVER, Since]),
  %Reply= get_onlineusers_count(),
  Reply = unique_count(), 
  error_logger:info_msg("~p:handle_call list_online count: ~p~n",
  	[?SERVER, Reply]),
  {reply, Reply, State};

handle_call(sync_session_from_cluster, _From, State)->
  Reply= user_presence_db:join_as_slave(State#state.cluster_master, [session]), 
  {reply, Reply, State};

handle_call(ping, _From, State) ->
  {reply, pong, State};

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
  Reply = {error, function_clause},
  {reply, Reply, State}.

-spec handle_cast(tuple(), state()) -> {noreply, state()}.
handle_cast(Info, State) ->
  erlang:display(Info),
  {noreply, State}.

handle_info({query_all_online}, State)->
  %Reply = set_user_webpresence(),
  Cluster = State#state.cluster_master,
  case user_presence_db:reach_node(Cluster) of
       {ok, reachable} -> 
       		 error_logger:info_msg("Cluster reachable ~p",[Cluster]),
			 sync_with_cluster(Cluster);
       {error, unreachable} -> error_logger:info_msg("Cluster unreachable ~p",[Cluster])
  end, 
  erlang:send_after(State#state.refresh_interval,
  	 self(), {query_all_online}),
  {noreply, State};

handle_info({list_all_online, Start}, State)->
  error_logger:info_msg("~p:handle_info(list_all_online) since:~p~n",
				[?SERVER, Start]),
  Reply = get_active_users_count(),
  error_logger:info_msg("Total members online ~p",[Reply]),
  Now = app_util:os_now(),
  erlang:send_after(State#state.refresh_interval,
     self(), {list_all_online, Now}),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

-spec handle_info(tuple(), pid(), state()) -> {ok, state()}.
handle_info(stop, _From, State)->
  terminate(normal, State).

terminate(_Reason, _State)->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

list_online_users() ->
    Users = get_all_sessions(),
    Count = length(Users),
    error_logger:info_msg("All online users: ~p~n", [Users]),
    {Count, Users}.

get_all_sessions() ->
   L = get_all_session_list(), 
   UList = [make_jid(U,S) || {{U, S, _R}, '_', '_' ,'_'} <- L],
   lists:usort(UList).

get_all_sessions(VHost) ->
   L = get_all_session_list(), 
   UList = [make_jid(U,S) || {{U, S, _R}, '_', '_' ,'_'} <- L, (VHost =:= S)],
   lists:usort(UList).

get_user_sessions(User, Server) ->
   L = get_all_sessions(User, Server),
   error_logger:info_msg("Get all online users list: ~p. Query for ~p@~p~n", [L, User, Server]), 
	L.

make_jid(U,S)-> lists:concat([U,"@",S]).
	
get_all_sessions(User, Server) ->
   error_logger:info_msg("~p:get_all_sessions: User:~p Server:~p~n",
		[?MODULE, User, Server]),
   User0 = binary_to_list(User),
   Server0 = binary_to_list(Server),
   Ret = mnesia:dirty_index_read(session, {User0, Server0}, #session.us),
   Ret1 = length(Ret),
   error_logger:info_msg("~p:get_session: length:~p~n",[?MODULE,Ret1]),
   {ok, Ret1}.

get_onlineusers_count()->
   length(get_all_sessions()).

get_all_session_list() ->
   mnesia:activity(async_dirty,
     fun() ->
            mnesia:foldl(fun(#session{usr = Usr}, AccIn) ->
                    [{Usr, '_' , '_', '_'} |AccIn]
                end,
                [],
                session)
     end).


kick_sessions(User, Server, Reason) ->
  lists:map(
     fun(Resource) ->
         kick_session(User, Server, Resource, Reason)
     end,
     get_resources(User, Server)).

kick_session(User, Server, Resource, Reason) ->
  ejabberd_router:route(
      jlib:make_jid("", "", ""),
      jlib:make_jid(User, Server, Resource),
      {xmlelement, "broadcast", [], [{exit, Reason}]}).

get_resources(User, Server) ->
  lists:map(
      fun(Session) ->
         element(3, Session#session.usr)
      end,
      get_sessions(User, Server)).

get_sessions(User, Server) ->
  LUser = jlib:nodeprep(User),
  LServer = jlib:nameprep(Server),
  Sessions = mnesia:dirty_index_read(session, {LUser, LServer}, #session.us),
  true = is_list(Sessions),
  Sessions.

is_multiple_sessions(User, Server )->
  Sessions = get_sessions(User,Server),
  length(Sessions) > 0.	

-spec total_count() -> integer().
total_count() ->
  mnesia:table_info(session, size).

-spec unique_count() -> integer().
unique_count() ->
    compute_unique(mnesia:dirty_first(session),
                   sets:new()).

-spec compute_unique(term(), set()) -> integer().
compute_unique('$end_of_table', Set) ->
    sets:size(Set);
compute_unique(Key, Set) ->
    NewSet = case mnesia:dirty_read(session, Key) of
                 [Session] ->
                     sets:add_element(Session#session.us, Set);
                 _ ->
                     Set
             end,
    compute_unique(mnesia:dirty_next(session, Key), NewSet).

is_not_fresh_connect(Table)-> 
   check_table_exists(Table).

sync_with_cluster(Cluster) ->
  sync_with_cluster(session, Cluster).
sync_with_cluster(Table, Cluster) when is_atom(Cluster) ->
  case is_not_fresh_connect(Table) of
	false ->   
	     error_logger:info_msg("There is no master",[]),  
             user_presence_db:join(Cluster,[session]);
	true -> 
	     error_logger:info_msg("There is a master",[]) 
    
  end,

  Reply = get_users_with_active_session(),
  Reply1 = lists:flatten(Reply),
  error_logger:info_msg("List of members online ~p from cluster ~p",[Reply1, Cluster]).

check_table_exists(Table)->
  Tabs = mnesia:system_info(tables),
  lists:member(Table, Tabs).

get_active_users_count() ->
  mnesia:table_info(session, size).

get_users_with_active_session() ->
  mnesia:dirty_select(
      session,
      [{#session{us = '$1', _ = '_'},
    [],
    ['$1']}]).
    

check_if_user_with_active_session(Jid, LServer)->
    US = {Jid, LServer},
    try
	    case mnesia:dirty_index_read(session, US, #session.us) of
			[] -> <<"offline">>;
			Ss -> <<"online">>
    	end
    catch 
    	_Class:_Reason -> <<"offline">>
    end.


set_user_webpresence()->
   Users = get_users_with_active_session(), 
   lists:map(fun(U) -> update_web_presence(U) end, Users).

read_session_from_ejabberd()->
  traverse_table_and_show(session).

traverse_table_and_show(Iterator, Table_name) when is_function(Iterator) ->
     case mnesia:is_transaction() of
        true -> mnesia:foldl(Iterator,[],Table_name);
        false -> 
            Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
            mnesia:activity(transaction,
              Exec,[{Iterator,Table_name}],
              mnesia_frag)
    end.   
traverse_table_and_show(Table_name)->
    Iterator = set_user_webpresence(),
    traverse_table_and_show(Iterator, Table_name).

create_config_table()->
  Start = app_util:os_now(),
  Ret = case mnesia:create_schema([node()]) of
  	ok -> ok = app_util:start_app(mnesia),
      	      error_logger:info_msg("Create mod_spark_rabbit_config table", []),

  		{atomic, ok} = mnesia:create_table(user_webpresence,
  							[{ram_copies, [node()]},
  							{type, set},
  							{attribute, record_info(fields, user_webpresence)}
  							]
  			);
  	{error,{S, {already_exists, S}}} -> 
        error_logger:warn_msg("Failure to create_schema: ~p", [{S, {already_exists, S}}]),
        %ok = should_delete_schema(Schema),
        ok = app_util:start_app(mnesia);
    Else ->
        error_logger:info_msg("Failure to create_schema: ~p", [Else]),
        ok = app_util:start_app(mnesia)
  end,
  End = app_util:os_now(),
  error_logger:info_msg("Create user_presence table ~p Start ~p End ~p", [?SERVER, Start, End]),
  Ret.

should_delete_schema(Schema) ->
  error_logger:info_msg("Delete schema ~p", [Schema]),
  catch(mnesia:stop()),
  %app_util:stop_app(mnesia),
  ok = mnesia:delete_schema([Schema]),
  % error_logger:info_msg("Deleted schema ~p ", [Ret]),
  ok.

user_with_active_session(Jid) ->
  user_with_active_session(Jid, 0).

user_with_active_session(Jid, Since) ->
  Ret = case mnesia:dirty_read({user_webpresence, Jid}) of
  	 [] -> nothing;
  	 [{user_webpresence, Jid , _, online, Last }] when Since >= Last
  	   -> {Jid, online};
  	 _ -> {Jid, not_found}
  end.
all_users_with_active_session(Since)->
   case all_users_with_active_session(Since) of 
   		{aborted, _} -> [];
   		L -> L
   end.
all_users_with_active_session0(Since) ->
   try
	   F = all_users_with_active_session0(session, Since),
   	   mnesia:transaction(F)
   catch 
   	   C:R -> []
   end.

all_users_with_active_session0(Table, Since) ->
  FilterFor = fun(Table)->
    qlc:eval(

      [X || X <- mnesia:table(Table)]
    )
  end.
  
transform(nothing) -> {ok, []};
transform([]) -> {ok, []};
transform(OnlineUsers) ->
  error_logger:info_msg("Transform onlineusers set:~p~n",[OnlineUsers]),
  Ret = lists:map(
  	fun(X)-> 
  		app_util:ensure_string(X)
  	end,OnlineUsers),
  {ok, Ret}.

dirty_get_us_list() ->
    Users = mnesia:dirty_select(
      session,
      [{#session{us = '$1', _ = '_'},
    [],
    ['$1']}]),
    lists:map(fun(U)-> update_web_presence(U) end, Users).

update_web_presence(User) ->
  [MemberId, BrandId] = get_login_data(User),
  Token = generate_token(),
  mnesia:dirty_write({user_webpresence, MemberId, online, Token}),
  ok.

get_login_data(User)->
  [<<"">>,<<"">>].


generate_token() ->
   R = app_util:os_now(),
   calendar:datetime_to_gregorian_seconds(R).
