-module(app_config_util).

-export([
	cwd/0,
	config_val/3,
	load_config/0,
	load_config/1,
	load_config/2,
	load_config_file/1, 
	app_get_env/3
]).

-define(CONFPATH,"conf").

-spec load_config()-> list().
load_config()->
  {ok, ConfDir}= cwd(),
  load_config(ConfDir, "spark_consumer.config").

-spec load_config(string())-> list().
load_config(File) ->
  {ok, ConfDir}= cwd(),
  load_config(ConfDir,File).

-spec load_config(string(), string())-> list().
load_config(ConfDir,File) when is_list(ConfDir), 
			  is_list(File)->
  FileFullPath = lists:concat([ConfDir,"/", File]),
  error_logger:info_msg("[~p]: Loading config: ~p",[?MODULE, FileFullPath]),
  {ok, [ConfList]}= file:consult(FileFullPath),
  {ok, [ConfList]}.

load_config_file(FileFullPath) when is_list(FileFullPath)->
   error_logger:info_msg("[~p]: Loading config: ~p",[?MODULE, FileFullPath]), 
  {ok, [ConfList]} = file:consult(FileFullPath),

  Conf = lists:flatten(ConfList),
  error_logger:info_msg("[~p]: Read config: ~p",[?MODULE, Conf]), 
  {ok, [ConfList]}.

-spec cwd()-> {ok, string()}.
cwd()->
  {ok, Cwd} = file:get_cwd(),
  {ok, lists:concat([Cwd,"/",?CONFPATH])}.

-spec config_val(atom(), list(), any()) -> any().
config_val(Key, Params, Default) -> {ok, proplists:get_value(Key, Params, Default)}.

app_get_env(App,Key,Default) when is_atom(App); is_atom(Key) ->
  case application:get_env(App,Key) of
  	 {ok, Val} -> Val;
  	 undefined -> Default
  end.
