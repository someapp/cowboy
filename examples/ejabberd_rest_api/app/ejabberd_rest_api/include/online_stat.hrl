-ifndef(ONLINE_STAT_HRL).
-define(ONLINE_STAT_HRL, true).
-record(online_stat, {
		count ::bitstring(),
		time_stamp ::bitstring()
}).

-export_records([online_stat]).
-endif.
