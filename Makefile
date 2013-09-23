# See LICENSE for licensing information.

PROJECT = cowboy

# Options.

COMPILE_FIRST = cowboy_middleware cowboy_sub_protocol
CT_SUITES = eunit http spdy ws
PLT_APPS = crypto public_key ssl

# Dependencies.

DEPS = cowlib ranch
dep_cowlib = pkg://cowlib 0.1.0
dep_ranch = pkg://ranch 0.8.5
dep_parse_trans = pkg://parse_trans 2.6
dep_jsonrec = pkg://edown  
dep_exmpp = pkg:exmpp 0.9.9
dep_goldrush = pkg:goldrush 20130828
dep_lager = pkg:lager 2.0.0rc2

TEST_DEPS = ct_helper gun
dep_ct_helper = https://github.com/extend/ct_helper.git master
dep_gun = pkg://gun master

# Standard targets.

include erlang.mk

# Extra targets.

.PHONY: autobahn

autobahn: clean clean-deps deps app build-tests
	@mkdir -p logs/
	@$(CT_RUN) -suite autobahn_SUITE
