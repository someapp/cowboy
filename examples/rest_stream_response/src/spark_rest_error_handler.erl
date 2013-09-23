-module (spark_rest_error_handler).
-export([onrequest_hook/1]).
-export([onresponse_hook/4]).


onrequest_hook(Req) ->
	erlang:display(<<"On Request Hook">>),
	erlang:display(Req),
	Req.

onresponse_hook(404, Headers, <<>>, Req) ->
	{Path, Req2} = cowboy_req:path(Req),
	Body = ["404 Not Found: \"", Path,
		"\" is not the path you are looking for.\n"],
	Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
		{<<"content-length">>, integer_to_list(iolist_size(Body))}),
	{ok, Req3} = cowboy_req:reply(404, Headers2, Body, Req2),
	Req3;
onresponse_hook(Code, Headers, <<>>, Req) when is_integer(Code), Code >= 400 ->
	Body = ["HTTP Error ", integer_to_list(Code), $\n],
	Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
		{<<"content-length">>, integer_to_list(iolist_size(Body))}),
	{ok, Req2} = cowboy_req:reply(Code, Headers2, Body, Req),
	Req2;
onresponse_hook(Code, Headers, Body, Req) ->
	erlang:display(<<"On Response Hook">>),
	erlang:display(Code),
	erlang:display(Headers),
	erlang:display(Body),
	erlang:display(Req),
	Req.
	
	
