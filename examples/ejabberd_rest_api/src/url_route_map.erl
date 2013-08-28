-module(url_route_map).
-export([
		route_map/1,
		route_map/2
]).

route_map(Opts)->
  route_map('socialstream.spark.net', Opts).

route_map(Host, Opts) when is_atom(Host)->
  [	
  	{Host,
  		[
  			{"/online/stat/total/", ejabberd_online_stat_handler, Opts},
			{"/online/user/jid", ejabberd_online_user_handler, Opts},
			{"/online/user/", ejabberd_online_user_set_handler, Opts},
			{"/", default_handler,[]}
		]
	}
  ].
