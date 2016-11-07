REBAR ?= "$(PWD)/bin/rebar"

compile:
	@$(REBAR) compile

escriptize: compile
	@$(REBAR) escriptize

clean:
	@$(REBAR) clean

dialyze:
	@$(REBAR) compile
	@$(REBAR) check-plt
	@$(REBAR) dialyze

test-shell:
	@RUNENV=test $(REBAR) shell

test-get-deps:
	@RUNENV=test $(REBAR) get-deps

test-compile:
	@RUNENV=test $(REBAR) compile

eunit:
	@RUNENV=test $(REBAR) eunit

ct:
	@RUNENV=test ct_run -spec test/ct/spec -logdir test/ct/spec_out -pa ./ebin -pa ./deps/*/ebin

ct-cover:
	@RUNENV=test ct_run -spec test/ct/spec -cover test/ct/coverspec -logdir test/ct/spec_out -pa ./ebin -pa ./deps/*/ebin

ct-clean:
	@rm -fr test/ct/spec_out/*

test-all: test-compile ct eunit
