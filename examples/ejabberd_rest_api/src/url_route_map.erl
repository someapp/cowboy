-module(url_route_map).
-export([
		route_map/1,
		route_map/2, 
		route_map/3
]).

-spec route_map(list()) -> list().
route_map(Opts)->
  route_map(Opts, []).

-spec route_map(list(),list()) -> list().

route_map(Opts, ConfigHosts)->
  Host = 'socialstream.spark.net',
  route_map(Host, Opts, ConfigHosts).

-spec route_map(list(),list(),list()) -> list().
route_map(Host, Opts, ConfigHosts)->
  [	
  	{Host,
  		[
  			{"/online/stat/total/", ejabberd_online_stat_handler, Opts},
			{"/online/user/", ejabberd_online_user_handler, Opts},
			{"/online/user/total/", 
			     ejabberd_online_user_set_handler, Opts}
		,{"/*", cowboy_static, {fun mimetypes:path_to_mimes/2, default}}
			
		]
	}
  ].


