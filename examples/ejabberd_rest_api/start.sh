#!/bin/sh_

erl -setcookie HJZSASYSTNSEITWLIYOU -name socialstream@`hostname -f` -pa ebin deps/*/ebin -boot start_sasl -s ejabberd_rest_api start -a -b
