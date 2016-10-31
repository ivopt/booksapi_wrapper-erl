compile:
	@rebar compile

escriptize: compile
	@rebar escriptize

clean:
	@rebar clean

clean-ct:
	@rm -fr spec/spec_out/*

test: clean
	@RUNENV=test rebar compile
	@RUNENV=test ct_run -spec spec/spec -logdir spec/spec_out -pa ./ebin -pa ./deps/*/ebin

test-cover: clean
	@RUNENV=test rebar compile
	@RUNENV=test ct_run -spec spec/spec -cover spec/coverspec -logdir spec/spec_out -pa ./ebin -pa ./deps/*/ebin

test-get-deps:
	@RUNENV=test rebar get-deps
