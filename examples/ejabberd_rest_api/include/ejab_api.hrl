-ifndef(EJAB_API_HRL).
-define(EJAB_API_HRL, true).

-define(FORWARD1(M, F), F(X) -> M:F(X)).
-define(FORWARD2(M, F), F(X, Y) -> M:F(X, Y)).
-define(FORWARD3(M, F), F(X, Y, Z) -> M:F(X, Y, Z)).

-record(session, {sid, usr, us, priority, info}).
-record(session_counter, {vhost, count}).

-export_records([session, session_counter]).
-endif.
