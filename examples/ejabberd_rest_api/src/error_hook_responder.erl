%% Feel free to use, reuse and abuse the code in this file.

-module(error_hook_responder).

-export([onrequest_hook/1]).
-export([onresponse_hook/3]).
-export([onresponse_hook/4]).
%-export([onresponse_hook/6]).
%-export([respond/4]).
-export([error_hook/4]).

onrequest_hook(Req)->
    {Method, Req2} = cowboy_req:method(Req),
    {Version, Req2} = cowboy_req:version(Req),
    {Host, Req2} = cowboy_req:host(Req),
    {Port, Req2} = cowboy_req:port(Req),
    {Cookies, Req3} = cowboy_req:cookies(Req),
    {Headers, Req2} = cowboy_req:headers(Req),
    {Qs, Req2} = cowboy_req:qs(Req),
    {Path, Req2} = cowboy_req:path(Req),
    {QsVals, Req4} = cowboy_req:qs_vals(Req),
    {Bindings, Req2} = cowboy_req:bindings(Req),
	Msg = io_lib:format("~n~n Started ~p ~p ~p for ~p ~p~n
                  qs_vals  : ~p~n
                  bindings : ~p~n
                  cookies  : ~p~n
                  headers  : ~p~n" ,
				[Method, Path, Qs, Host, 
                Port, QsVals, Bindings, 
                Cookies, Headers]),
	error_logger:info_msg(Msg,[]),
	Req.    


onresponse_hook(Code, Headers, Req) ->
    {Method, Req2} = cowboy_req:method(Req), 
    {Path,  Req2} = cowboy_req:path(Req),
    {QueryStringVals, Req3} = cowboy_req:qs_vals(Req),
    {Cookies, Req4} = cowboy_req:cookies(Req),
    {Host, Req2} = cowboy_req:host(Req),
    {Port, Req2} = cowboy_req:port(Req),
    Msg = io_lib:format("~n~n Completed ~p ~p ~p ~p for ~p ~p~n
        				cookies  : ~p~n
        				headers  : ~p~n", 
      				[Code, Method, 
      				 Path, QueryStringVals, 
      				 Host, Port, 
      				 Cookies, Headers]),
    error_logger:info_msg(Msg,[]),
    Req.

onresponse_hook(Code, Headers, Req, Body) ->
	respond(Code, Headers, Body, Req).

respond(404, Headers, <<>>, Req) ->
	{Path, Req2} = cowboy_req:path(Req),
	Body = <<"404 Not Found: \"", Path/binary, "\" is not the path you are looking for.\n">>,
	Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
		{<<"content-length">>, body_length(Body)}),
	{ok, Req3} = cowboy_req:reply(404, Headers2, Body, Req2),
	Req3;
	
respond(Code, Headers, <<>>, Req) when is_integer(Code), Code >= 400 ->
	Body = ["HTTP Error ", integer_to_list(Code), $\n],
	Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
		{<<"content-length">>, body_length(Body)}),
	{ok, Req2} = cowboy_req:reply(Code, Headers2, Body, Req),
	Req2;
	
respond(Code, Headers, Body, Req) ->
    Code1 = code_to_bitstring(Code),
	Body1 = << Code1/binary, <<" Internal Server Error. \n">> /binary >>,
	
	Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
		{<<"content-length">>, body_length(Body1)}),
	error_logger:info_msg("Req is: ~p Body Length: ~p~n", 
			[Req, body_length(Body1)]),			
	{ok, Req1} = cowboy_req:path(Req),
    error_logger:info_msg("Req1: ~p~n",[Req1]),
    {ok, Req2} = cowboy_req:reply(500, Headers2, Body1, Req1),
	Req2.


%% +-----------------------------------------------------------------+
%% | PRIVATE FUNCTIONS                                               |
%% +-----------------------------------------------------------------+
%ensure_non_empty_req(<<>>) -> 
%ensure_non_empty_req(<<"">>) ->

%ensure_non_empty_req(Req) -> Req.

%make_cowboy_req(Req)->
	


code_to_bitstring(Code) when is_integer(Code)->
	list_to_binary(integer_to_list(Code));
code_to_bitstring(Code) when is_binary(Code)-> Code;
code_to_bitstring(Code) when is_list(Code)->
	list_to_binary(Code);
code_to_bitstring(Code) -> {error, badarg}.

body_length(<<>>) -> "0";
body_length(<<"">>) -> "0";
body_length(Body) when is_binary(Body)->
	integer_to_list(iolist_size(Body));
body_length(Body) when is_list(Body)->
	integer_to_list(iolist_size(Body)).


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
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
error_hook(404, Headers, <<>>, Req) ->
	{Path, Req2} = cowboy_req:path(Req),
	Body = ["404 Not Found: \"", Path,
		"\" is not the path you are looking for.\n"],
	Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
		{<<"content-length">>, integer_to_list(iolist_size(Body))}),
	{ok, Req3} = cowboy_req:reply(404, Headers2, Body, Req2),
	Req3;
error_hook(Code, Headers, <<>>, Req) when is_integer(Code), Code >= 400 ->
	Body = ["HTTP Error ", integer_to_list(Code), $\n],
	Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
		{<<"content-length">>, integer_to_list(iolist_size(Body))}),
	{ok, Req2} = cowboy_req:reply(Code, Headers2, Body, Req),
	Req2;
error_hook(_Code, _Headers, _Body, Req) ->
	Req.
    
    
    
    

