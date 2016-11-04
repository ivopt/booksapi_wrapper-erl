REBAR ?= "$(PWD)/bin/rebar"

compile:
	@$(REBAR) compile

escriptize: compile
	@$(REBAR) escriptize

clean:
	@$(REBAR) clean

test-get-deps:
	@RUNENV=test $(REBAR) get-deps

eunit:
	@RUNENV=test $(REBAR) eunit

ct: clean
	@RUNENV=test $(REBAR) compile
	@RUNENV=test ct_run -spec test/ct/spec -logdir test/ct/spec_out -pa ./ebin -pa ./deps/*/ebin

ct-cover: clean
	@RUNENV=test $(REBAR) compile
	@RUNENV=test ct_run -spec test/ct/spec -cover test/ct/coverspec -logdir test/ct/spec_out -pa ./ebin -pa ./deps/*/ebin

ct-clean:
	@rm -fr test/ct/spec_out/*

test-all: ct eunit
