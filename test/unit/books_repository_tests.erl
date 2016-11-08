-module(books_repository_tests).
-include("src/options.hrl").
-include("src/book.hrl").
-include_lib("eunit/include/eunit.hrl").

search_results_test_() -> {
  setup,
  fun given/0,
  fun stop/1,
  fun search_results_tests/1
}.


given() ->
  books_repository:start("RandomURL"),
  meck:new(hackney, [non_strict]),
  meck:expect(hackney, get, fun(_Url, [], "", []) -> { ok, 200, [], fake_client } end),
  {ok, Body} = file:read_file("../test/unit/booksapi_response.json"),
  meck:expect(hackney, body, fun(fake_client) -> { ok, Body } end),
  {hackney, books_repository:search_books(#options{})}.

stop({Mock, _}) ->
  meck:unload(Mock).

search_results_tests({_, Results}) -> [
  { "Search returns a list of 10 elements",
    ?_assertEqual(10, length(Results))},

  { "Search returns a list of books",
    [?_assertMatch(#book{}, Row) || Row <- Results]}
].
