-module(url_route_map).
-export([
		route_map/1,
		route_map/2, 
		route_map/3,
		route_map/4
]).

-define(FORWARD1(M, F), F(X) -> M:F(X)).
-define(FORWARD2(M, F), F(X, Y) -> M:F(X, Y)).
-define(FORWARD3(M, F), F(X, Y, Z) -> M:F(X, Y, Z)).
-define(ONLINE_STAT, 


-spec route_map(list()) -> list().
route_map(Opts)->
  route_map(Opts, []).

-spec route_map(list(),list()) -> list().

route_map(Opts, ConfigHosts)->
  Host = 'socialstream.spark.net',
  route_map(Host, Opts, ConfigHosts).

route_map(Host, Opts, ConfigHosts)->
  route_map(Host, [], 
  			'ejabberd_online_stat', ConfigHosts).
   					
-spec route_map(list(),list(),list()) -> list().
route_map(Host, Opts, Handler, _ConfigHosts)->
  cowboy_router:compile(
  [	
  	{Host,
  		[
  			{"/[:v1]/online/stat/total/", [{v1, int}], 
  				get_online_stat_set_handler(Handler), Opts},
			{"/[:v1]/online/user/", [{v1, int}],
				get_online_user_handler(Handler), Opts},
			{"/[:v1]/online/user/total/", [{v1, int}],
			    get_online_user_set_handler(Handler), Opts}
			,{"/*", cowboy_static, {fun mimetypes:path_to_mimes/2, default}}
	%		,{'_', default_handler, Opts}		
		]
	},
	{'_', [{'_', default_handler, Opts }]}
  ]).

get_online_stat_set_handler('ejabberd_online_stat')->
	'ejabberd_online_stat_set_handler';
get_online_stat_set_handler('debug_toppage')->
	get_toppage().

get_online_stat_handler('ejabberd_online_stat')->
	'ejabberd_online_stat_handler';
get_online_stat_handler('debug_toppage')->
	get_toppage().

get_online_user_handler('ejabberd_online_stat')->
	'ejabberd_online_user_handler';
get_online_user_handler('debug_toppage')->
	get_toppage().

get_online_user_set_handler('ejabberd_online_stat')->
	'ejabberd_online_user_set_handler';
get_online_user_set_handler('debug_toppage')->
	get_toppage().


get_toppage()-> 'toppage_handler'.
