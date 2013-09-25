-ifndef(ONLINE_USER_HRL).
-define(ONLINE_USER_HRL, true).
-record(online_user, {
		jid ::string(),
		onlinenow  ::string(),
		time_stamp ::string()
}).


-export_records([online_user]).

-endif.
