-ifndef(ONLINE_USER_HRL).
-define(ONLINE_USER_HRL, true).
-record(online_user, {
		jid ::bitstring(),
		onlinenow  ::bitstring(),
		time_stamp ::bitstring()
}).


-export_records([online_user]).

-endif.
