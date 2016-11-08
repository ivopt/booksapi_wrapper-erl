-module(books_cli_tests).
-include_lib("eunit/include/eunit.hrl").
-include("src/book.hrl").

-define(_outputs(RegEx, Output), ?_assertMatch({match, _}, re:run(Output, RegEx)) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Run with results

start_mock_with_results() ->
  meck:new(mock_books_app, [non_strict]),
  meck:expect(mock_books_app, search, fun(_) -> [
    ?book(<<"Book1">>, <<"2016-11-02">>, [<<"Author 1">>, <<"Author 2">>]),
    ?book(<<"Book3">>, <<"2016-10-28">>, [<<"Author 2">>, <<"Author 3">>]),
    ?book(<<"Book2">>, <<"2016-11-01">>, [<<"Author 1">>, <<"Author 3">>])
  ] end),
  mock_books_app.


run_with_results_test_() -> {
  setup,
  fun start_mock_with_results/0,
  fun unload_mock/1,
  fun (Mock) -> [
    { "Without arguments, outputs the usage message",
      ?_outputs("^Usage: .*", books_cli:run([], {Mock}))},

    { "Outputs and Indexes the books by the order in which the books app responded",
      ?_outputs("1\\).*Book1(\\n.*)*.*2\\).*Book3(\\n.*)*.*3\\).*Book2", books_cli:run(["anything"], {Mock}))}
  ]
  end
}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Run with no results

start_mock_without_results() ->
  meck:new(mock_books_app, [non_strict]),
  meck:expect(mock_books_app, search, fun(_) -> [] end),
  mock_books_app.

run_without_results_test_() -> {
  setup,
  fun start_mock_without_results/0,
  fun unload_mock/1,
  fun (Mock) -> [
    { "Outputs an error message",
      ?_outputs("Your search criteria does match any book", books_cli:run(["giberishstuff"], {Mock}))}
  ]
  end
}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helpers

unload_mock(Mock) -> meck:unload(Mock).
