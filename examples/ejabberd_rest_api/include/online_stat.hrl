-ifndef(ONLINE_STAT_HRL).
-define(ONLINE_STAT_HRL, true).
-record(online_stat, {
		count :: string(),
		time_stamp :: string()
}).

-export_records([online_stat]).
-endif.
