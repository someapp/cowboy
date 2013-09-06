-ifndef(ONLINE_USER_SET_HRL).
-define(ONLINE_USER_SET_HRL, true).
-record(online_user_set, {
		count ::bitstring(),
		time_stamp ::bitstring(),
		jids :: [bitstring()]
}).

-export_records([online_user_set]).
-endif.
