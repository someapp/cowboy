-module(app_util).

-export([start_app/1, stop_app/1]).
-export([start_apps/1]).

-export([ensure_binary/1,
		 ensure_string/1, 
		 ensure_loaded/1]).

-export([is_process_alive/1]).

-export([os_now/0, timespan/2, get_printable_timestamp/0]).

-export([config_val/3]).

-export([to_integer/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start_app(ok)-> ok;
start_app(App) when is_atom(App) ->
	start_app(application:start(App));
start_app({error, {already_started, App}})
		when is_atom(App) -> ok;
start_app({error, {Reason, App}}) 
		when is_atom(App) ->
	{error, {Reason, App}};
start_app({E, {Reason, App}}) ->
	{E, {Reason, App}};
start_app(_)-> {error, badarg}.

start_apps([])-> ok;
start_apps(Apps) when is_list(Apps) ->
    lists:map(fun(App) -> 
   		ok = start_app(App),
		error_logger:info_msg("Started application ~p",[App])
   	end, Apps);
start_apps(_) -> {error, badarg}.

stop_app(ok)-> ok;
stop_app(stopped)->ok;
stop_app({error,{not_started,App}})
		when is_atom(App)-> ok;
stop_app({error, {Reason, App}}) 
		when is_atom(App) ->
	{error, {Reason, App}};
stop_app({E, {Reason, App}})-> 
	{E, {Reason, App}};
stop_app(_)-> {error, badarg}.

is_process_alive(Pid) when is_pid(Pid)->
	true = erlang:is_process_alive(Pid).

-spec ensure_binary(any())-> bitstring().
ensure_binary(undefined)-> undefined;
ensure_binary(Value) when is_binary(Value)->
	Value;
ensure_binary(Value) when is_list(Value)->
	Value0 = lists:flatten(Value),
	list_to_binary(Value0);
ensure_binary(Value) when is_integer(Value)->
	list_to_binary(integer_to_list(Value));
ensure_binary(Value) when is_atom(Value) ->
	erlang:atom_to_binary(Value, utf8).

ensure_string(Value) when is_list(Value) ->
	lists:flatten(Value);
	 
ensure_string(Value) -> 
	binary_to_list(ensure_binary(Value)).

-spec os_now() -> calendar:datetime1970().
os_now()->
  R =os:timestamp(),
  calendar:now_to_universal_time(R).

-spec get_printable_timestamp() -> string().
get_printable_timestamp() -> 
   get_printable_timestamp(string).

get_printable_timestamp(string) -> 
   Seconds = date_util:epoch(),
   {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:gregorian_seconds_to_datetime(Seconds),
   io_lib:fwrite("~2B/~2B/~4..0B ~2B:~2.10.0B:~2.10.0B", [Month, Day, Year, Hour, Min, Sec]);     
get_printable_timestamp(binary) ->
   R = get_printable_timestamp(string), 
   erlang:list_to_binary(R).   
   
-spec timespan( calendar:datetime1970(), calendar:datetime1970())-> calendar:datetime1970().
timespan(A,B)->
  calendar:time_difference(A,B).

-spec config_val(atom(), list(), any()) -> any().
config_val(Key, List, Default) -> {ok, proplists:get_value(Key, List, Default)}.

-spec ensure_loaded(atom()) -> {ok, loaded} | {error, any()}.
ensure_loaded(Mod) when is_atom(Mod)-> 
  case code:ensure_loaded(Mod) of
      {module, Mod} -> {ok, loaded};
      E -> {error, E}
  end;
ensure_loaded(_) -> {error, badarg}.

-spec to_integer(any()) -> integer() | {error, term()}.
to_integer(A) when is_integer(A) -> A;
to_integer(A) when is_binary(A)-> 
	list_to_integer(binary_to_list(A));
to_integer(A) when is_list(A)-> 
	list_to_integer(A);	
to_integer(A)->
	error_logger:error_msg("~p:to_integer: ~p wrong type~n",[A]),
	A.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).
app_helper_test_() ->
    { setup,
      fun setup/0,
      fun cleanup/1,
      [
%       {"Should start app",fun start_app_test/0},      
%       {"Should start app",fun start_app_test/0}	
      ]
    }.

setup() ->
    ok.

cleanup(_Ctx) ->
    ok.



-endif.
