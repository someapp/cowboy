# See LICENSE for licensing information.

PROJECT = cowboy

# Options.

<<<<<<< HEAD
# Makefile tweaks.

V ?= 0
erl_vsn = @echo "Erlang version: (requires at least 5.9)" `erl -v `
appsrc_verbose_0 = @echo " APP   " $(PROJECT).app.src;
appsrc_verbose = $(appsrc_verbose_$(V))

erlc_verbose_0 = @echo " ERLC  " $(?F);
erlc_verbose = $(erlc_verbose_$(V))

gen_verbose_0 = @echo " GEN   " $@;
gen_verbose = $(gen_verbose_$(V))

.PHONY: all clean-all app clean deps clean-deps docs clean-docs tests autobahn build-plt dialyze

# Application.

all: erlvsn deps app

clean-all: clean clean-deps clean-docs
	$(gen_verbose) rm -rf .$(PROJECT).plt $(DEPS_DIR) logs

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')

app: ebin/$(PROJECT).app
	$(appsrc_verbose) cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' \
		> ebin/$(PROJECT).app

COMPILE_FIRST = src/cowboy_middleware.erl src/cowboy_sub_protocol.erl

ebin/$(PROJECT).app: src/*.erl
	@mkdir -p ebin/
	$(erlc_verbose) erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ \
		$(COMPILE_FIRST) $?

clean:
	$(gen_verbose) rm -rf ebin/ test/*.beam erl_crash.dump
=======
COMPILE_FIRST = cowboy_middleware cowboy_sub_protocol
CT_SUITES = eunit http spdy ws
PLT_APPS = crypto public_key ssl
>>>>>>> eb4843a46b781f93030b623e2b1feb1898ba3da0

# Dependencies.

DEPS = cowlib ranch
dep_cowlib = pkg://cowlib 0.1.0
dep_ranch = pkg://ranch 0.8.5

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
