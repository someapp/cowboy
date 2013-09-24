-module(url_route_map).
-export([
		route_map/1,
		route_map/2, 
		route_map/3,
		route_map/4
]).

-spec route_map(list()) -> list().
route_map(Opts)->
  route_map(Opts, []).

-spec route_map(list(),list()) -> list().

route_map(Opts, ConfigHosts)->
  Host = 'socialstream.spark.net',
  route_map(Host, Opts, ConfigHosts).

route_map(Host, Opts, ConfigHosts)->
  route_map(Host, [], 
  			ejabberd_online_stat_handler, ConfigHosts).
   					
-spec route_map(list(),list(),list()) -> list().
route_map(Host, Opts, Handler, _ConfigHosts)->
  cowboy_router:compile(
  [	
  	{Host,
  		[
  			{"/[:v1]/online/stat/total/", [{v1, int}], 
  				Handler, Opts},
			{"/[:v1]/online/user/", [{v1, int}],
				Handler, Opts},
			{"/[:v1]/online/user/total/", [{v1, int}],
			    Handler, Opts}
			,{"/*", cowboy_static, {fun mimetypes:path_to_mimes/2, default}}
	%		,{'_', default_handler, Opts}		
		]
	},
	{'_', [{'_', default_handler, Opts }]}
  ]).


