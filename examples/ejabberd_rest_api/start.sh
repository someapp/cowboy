#!/bin/sh_
erl -pa ebin deps/*/ebin -s ejabberd_rest_api \
	-eval "io:format(\"Point your browser at http://localhost:8080/~n\")."
