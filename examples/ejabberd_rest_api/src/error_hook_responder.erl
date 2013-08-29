%% Feel free to use, reuse and abuse the code in this file.

-module(error_hook_responder).

-export([onrequest_hook/1]).
-export([onresponse_hook/3]).
-export([respond/4]).

onrequest_hook(Req)->
    {Method, Req2} = cowboy_req:method(Req),
    {Version, Req2} = cowboy_req:version(Req),
    {Host, Req2} = cowboy_req:host(Req),
    {Port, Req2} = cowboy_req:port(Req),
    {Cookies, Req2} = cowboy_req:cookies(Req),
    {Headers, Req2} = cowboy_req:headers(Req),
    {Qs, Req2} = cowboy_req:qs(Req),
    {QsVals, Req2} = params_to_string(cowboy_req:qs_vals(Req)),
    {Bindings, Req2} = cowboy_req:bindings(Req),
    {Path, Req2} = cowboy_req:path(Req),
	error_logger:info_msg("~n~nStarted " ++ Method ++ " " ++ Path ++ QsVals ++ " for " ++ Host ++ Port ++ "~n"
               "  qs_vals  : " ++ to_native_string(QsVals) ++ "~n"
               "  raw_qs   : " ++ to_native_string(Qs) ++ "~n"
               "  bindings : " ++ to_native_string(Bindings) ++ "~n"
               "  cookies  : " ++ to_native_string(Cookies) ++ "~n"
               "  headers  : " ++ to_native_string(Headers)),
	Req.    


onresponse_hook(Code, Headers, Req) ->

    {Method, Req2} = cowboy_req:method(Req), 
    {Path,  Req2} = cowboy_req:path(Req),
    {Params, Req2} = params_to_string(cowboy_req:qs_vals(Req)),
    {Cookies, Req2} = cowboy_req:cookies(Req),
    {Host, Req2} = cowboy_req:host(Req),
    {Port, Req2} = pcowboy_req:port(Req),
    error_logger:info_msg(
      "~n~nCompleted " ++ to_string(Code) ++ " " ++ Method ++ " " ++ Path ++ Params ++ " for " ++ Host ++ Port ++ "~n"
      "  cookies  : " ++ to_native_string(Cookies) ++ "~n"
      "  headers  : " ++ to_native_string(Headers)),
    Req.




respond(404, Headers, <<>>, Req) ->
	{Path, Req2} = cowboy_req:path(Req),
	Body = <<"404 Not Found: \"", Path/binary, "\" is not the path you are looking for.\n">>,
	Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
		{<<"content-length">>, integer_to_list(byte_size(Body))}),
	{ok, Req3} = cowboy_req:reply(404, Headers2, Body, Req2),
	Req3;
respond(Code, Headers, <<>>, Req) when is_integer(Code), Code >= 400 ->
	Body = ["HTTP Error ", integer_to_list(Code), $\n],
	Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
		{<<"content-length">>, integer_to_list(iolist_size(Body))}),
	{ok, Req2} = cowboy_req:reply(Code, Headers2, Body, Req),
	Req2;
respond(_Code, _Headers, _Body, Req) ->
	Req.


%% +-----------------------------------------------------------------+
%% | PRIVATE FUNCTIONS                                               |
%% +-----------------------------------------------------------------+

params_to_string(Params) ->
    case to_string(Params) of
        "" -> "";
        OtherParams -> "?" ++ OtherParams
    end.

port_to_string(Port) ->
    case to_string(Port) of
        "80" -> "";
        OtherPort -> ":" ++ OtherPort
    end.

%% print value as standard format
to_native_string(Value) ->
    io_lib:format("~p", [Value]).

%% convert everything to string
to_string(undefined) ->
    "";
to_string(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_string(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
to_string(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
to_string([])
-> "";
to_string(List) when is_list(List) ->
    to_string(List, "").

%% convert lists to string
to_string(List, Separator) when is_list(List) ->
    string:join(list_to_string(List, []), Separator).

list_to_string([], Result) ->
    lists:reverse(Result);
list_to_string([Head| Rest], Result) ->
    list_to_string(Rest, [to_string(Head)|Result]).
