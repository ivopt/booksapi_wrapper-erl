-module(books_util_tests).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% to_list tests
to_list_test_() -> [
  ?_assertEqual([],              books_util:to_list([])),
  ?_assertEqual("hello world",   books_util:to_list("hello world")),
  ?_assertEqual("hello world",   books_util:to_list(<<"hello world">>)),
  ?_assertError(function_clause, books_util:to_list(100))
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% list_as_string tests
list_as_string_test_() -> [
  ?_assertEqual("",       books_util:list_as_string([])),
  ?_assertEqual("apple",  books_util:list_as_string(["apple"])),
  ?_assertEqual("abcdef", books_util:list_as_string(["ab", "cd", "ef"])),
  ?_assertEqual("abcdef", books_util:list_as_string([<<"abc">>, "def"])),
  ?_assertError(function_clause, books_util:list_as_string(100))
].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% join tests
join_test_() -> [
  ?_assertEqual("",             books_util:join("", [])),
  ?_assertEqual("hello",        books_util:join(",", ["hello"])),
  ?_assertEqual("hello, world", books_util:join(", ", ["hello", "world"]))
].
