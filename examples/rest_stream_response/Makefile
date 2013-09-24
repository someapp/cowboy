PROJECT = rest_stream_response

DEPS = cowlib ranch parse_trans json_rec edown exmpp goldrush lager
dep_cowlib = pkg://cowlib 0.1.0
dep_ranch = pkg://ranch 0.8.5
dep_parse_trans = https://github.com/uwiger/parse_trans.git master
dep_json_rec = https://github.com/justinkirby/json_rec.git master 
dep_edown = https://github.com/esl/edown.git master
dep_exmpp = https://github.com/processone/exmpp.git master
dep_goldrush = https://github.com/DeadZen/goldrush.git master
dep_lager = https://github.com/basho/lager.git master

.PHONY: release clean-release

release: clean-release all
	relx

submake: 
	@(cd deps/edown; make)
	@(cd deps/exmpp; make)

clean-release:
	rm -rf _rel

include erlang.mk
