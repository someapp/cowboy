-ifndef(ONLINE_USER_SET_HRL).
-define(ONLINE_USER_SET_HRL, true).
-record(online_user_set, {
		count ::string(),
		time_stamp ::string(),
		jids :: [string()]
}).

-export_records([online_user_set]).
-endif.
