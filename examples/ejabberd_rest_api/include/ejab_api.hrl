-ifndef(EJAB_API_HRL).
-define(EJAB_API_HRL, true).

-define(?FORWARD1(M, F), F(X) -> M:F(X)).
-define(?FORWARD2(M, F), F(X,Y) -> M:F(X,Y)).
-define(?FORWARD3(M, F), F(X,Y,Z) -> M:F(X,Y,Z)).

?FORWARD1(tty, error_logger:tty). 
?FORWARD2(display, erlang:display). 
?FORWARD2(warn, error_logger:warning_msg). 
?FORWARD2(error, error_logger:error_msg). 
?FORWARD2(info, error_logger:info_msg). 



-endif.
