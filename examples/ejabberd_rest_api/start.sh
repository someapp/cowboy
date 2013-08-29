#!/bin/sh_
erl -pa ebin deps/*/ebin -s ejabberd_rest_api start
