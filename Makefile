compile:
	@rebar compile

escriptize: compile
	@rebar escriptize

clean:
	@rebar clean

test-get-deps:
	@RUNENV=test rebar get-deps

eunit:
	@RUNENV=test rebar eunit

ct: clean
	@RUNENV=test rebar compile
	@RUNENV=test ct_run -spec test/ct/spec -logdir test/ct/spec_out -pa ./ebin -pa ./deps/*/ebin

ct-cover: clean
	@RUNENV=test rebar compile
	@RUNENV=test ct_run -spec test/ct/spec -cover test/ct/coverspec -logdir test/ct/spec_out -pa ./ebin -pa ./deps/*/ebin

ct-clean:
	@rm -fr test/ct/spec_out/*

test-all: ct eunit
