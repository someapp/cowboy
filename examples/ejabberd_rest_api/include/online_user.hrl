-ifndef(ONLINE_USER_HRL).
-define(ONLINE_USER_HRL, true).
-record(ONLINE_USER, {
		jid ::bitstring(),
		onlinenow  ::bitstring(),
		time_stamp ::bitstring()
}).


-export_records([ONLINE_USER]).

-endif.
