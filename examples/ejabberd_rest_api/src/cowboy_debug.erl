%% See LICENSE for licensing information.
-module(cowboy_debug).

%-include_lib("cowboy/include/http.hrl").

-export([onrequest_hook/1,
         onresponse_hook/3]).

%% +-----------------------------------------------------------------+
%% | HOOKS FUNCTIONS                                                 |
%% +-----------------------------------------------------------------+
-type cookie_option() :: {max_age, non_neg_integer()}
	| {domain, binary()} | {path, binary()}
	| {secure, boolean()} | {http_only, boolean()}.
-type cookie_opts() :: [cookie_option()].
-export_type([cookie_opts/0]).

-type resp_body_fun() :: fun((inet:socket(), module()) -> ok).

-record(http_req, {
	%% Transport.
	socket = undefined :: undefined | inet:socket(),
	transport = undefined :: undefined | module(),
	connection = keepalive :: keepalive | close,

	%% Request.
	pid = undefined :: pid(),
	method = <<"GET">> :: binary(),
	version = {1, 1} :: cowboy_http:version(),
	peer = undefined :: undefined | {inet:ip_address(), inet:port_number()},
	host = undefined :: undefined | binary(),
	host_info = undefined :: undefined | cowboy_router:tokens(),
	port = undefined :: undefined | inet:port_number(),
	path = undefined :: binary(),
	path_info = undefined :: undefined | cowboy_router:tokens(),
	qs = undefined :: binary(),
	qs_vals = undefined :: undefined | list({binary(), binary() | true}),
	fragment = undefined :: binary(),
	bindings = undefined :: undefined | cowboy_router:bindings(),
	headers = [] :: cowboy_http:headers(),
	p_headers = [] :: [any()], %% @todo Improve those specs.
	cookies = undefined :: undefined | [{binary(), binary()}],
	meta = [] :: [{atom(), any()}],

	%% Request body.
	body_state = waiting :: waiting | done | {stream,
		non_neg_integer(), non_neg_integer(), fun(), any(), fun()},
	multipart = undefined :: undefined | {non_neg_integer(), fun()},
	buffer = <<>> :: binary(),

	%% Response.
	resp_compress = false :: boolean(),
	resp_state = waiting :: locked | waiting | chunks | done,
	resp_headers = [] :: cowboy_http:headers(),
	resp_body = <<>> :: iodata() | resp_body_fun()
		| {non_neg_integer(), resp_body_fun()},

	%% Functions.
	onresponse = undefined :: undefined | already_called
		| cowboy_protocol:onresponse_fun()
}).

onrequest_hook(Req) ->
    Method = to_string(Req#http_req.method),
    Path = to_string(Req#http_req.path),
    Params = params_to_string(Req#http_req.qs_vals),
    Host = to_string(Req#http_req.host, "."),
    Port = port_to_string(Req#http_req.port),
    lager:info("~n~nStarted " ++ Method ++ " " ++ Path ++ Params ++ " for " ++ Host ++ Port ++ "~n"
               "  qs_vals  : " ++ to_native_string(Req#http_req.qs_vals) ++ "~n"
               "  raw_qs   : " ++ to_native_string(Req#http_req.qs) ++ "~n"
               "  bindings : " ++ to_native_string(Req#http_req.bindings) ++ "~n"
               "  cookies  : " ++ to_native_string(Req#http_req.cookies) ++ "~n"
               "  headers  : " ++ to_native_string(Req#http_req.headers)),
    Req.

onresponse_hook(Code, Headers, Req) ->
    Method = to_string(Req#http_req.method),
    Path = to_string(Req#http_req.path),
    Params = params_to_string(Req#http_req.qs_vals),
    Host = to_string(Req#http_req.host, "."),
    Port = port_to_string(Req#http_req.port),
    lager:info(
      "~n~nCompleted " ++ to_string(Code) ++ " " ++ Method ++ " " ++ Path ++ Params ++ " for " ++ Host ++ Port ++ "~n"
      "  cookies  : " ++ to_native_string(Req#http_req.cookies) ++ "~n"
      "  headers  : " ++ to_native_string(Headers)),
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
