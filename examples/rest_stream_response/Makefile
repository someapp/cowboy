PROJECT = rest_stream_response


DEPS = cowboy edown exmpp goldrush lager
dep_cowboy = pkg://cowboy master
dep_edown = https://github.com/esl/edown.git master
dep_exmpp = https://github.com/processone/exmpp.git v0.9.9
dep_goldrush = https://github.com/DeadZen/goldrush.git master
dep_lager = https://github.com/basho/lager.git master



.PHONY: release clean-release

release: clean-release all
	relx

clean-release:
	rm -rf _rel

include ../../erlang.mk
