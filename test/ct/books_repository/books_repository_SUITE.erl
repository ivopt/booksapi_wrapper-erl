-module(books_repository_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("../../../src/options.hrl").

-export([all/0, groups/0]).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests Configuration
all() -> [{group, search_books}].

groups() -> [{
  search_books, [], [
    searching_non_existing_books_returns_an_empty_list
  ]
}].

init_per_suite(Config) ->
  application:start(books),
  Config.

end_per_suite(_Config) ->
  application:stop(books),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

searching_non_existing_books_returns_an_empty_list(_) ->
  Options = #options{author="NonExistingDude", title="DefinitlyFakeTitle", free_term="qwertyuioplkjhgfdsa"},
  [] = books:search(Options).
