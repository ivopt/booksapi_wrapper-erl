-module(books_cli_tests).
-include_lib("eunit/include/eunit.hrl").
-define(_outputs(RegEx, Output), ?_assertMatch({match, _}, re:run(Output, RegEx)) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Run

run_test_() -> {
  setup,
  fun start/0,
  fun stop/1,
  fun run_tests/1
}.

start() ->
  meck:new(mock_books_app, [non_strict]),
  meck:expect(mock_books_app, search, fun(_) -> [
    {<<"Book1">>, <<"2016-11-02">>, [<<"Author 1">>, <<"Author 2">>]},
    {<<"Book3">>, <<"2016-10-28">>, [<<"Author 2">>, <<"Author 3">>]},
    {<<"Book2">>, <<"2016-11-01">>, [<<"Author 1">>, <<"Author 3">>]}
  ] end),
  mock_books_app.

stop(Mock) ->
  meck:unload(Mock).

run_tests(Mock) -> [
  { "Without arguments, outputs the usage message",
    ?_outputs("^Usage: .*", books_cli:run([], {Mock}))},

  { "Outputs and Indexes the books by the order in which the books app responded",
    ?_outputs("1\\).*Book1(\\n.*)*.*2\\).*Book3(\\n.*)*.*3\\).*Book2", books_cli:run(["anything"], {Mock}))}
].
