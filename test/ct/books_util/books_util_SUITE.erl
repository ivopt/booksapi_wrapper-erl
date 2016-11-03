-module(books_util_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0]).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests Configuration
all() -> [{group, all}].

groups() ->
  [
    {all, [parallel], [{group, to_list}, {group, list_as_string}, {group, join}]},
    {
      to_list, [parallel], [
        given_a_list_returns_that_list,
        given_a_list_string_returns_that_string,
        given_a_binary_string_returns_its_list_string_representation,
        to_list_blows_up_if_argument_not_a_string
      ]
    },
    {
      list_as_string, [parallel], [
        given_empty_list_returns_empty_string,
        given_a_list_with_one_string_returns_that_string,
        given_a_list_with_many_strings_returns_a_concatenation,
        given_a_list_that_contains_binary_strings_the_result_is_a_list_string,
        list_as_string_blows_up_if_argument_not_a_list
      ]
    },
    {
      join, [parallel], [
        join_empty_list_just_returns_empty_string,
        join_list_with_single_string_returns_that_string,
        join_list_with_more_than_1_string_returns_those_strings_separated_by_the_separator
      ]
    }
  ].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% to_list tests
given_a_list_returns_that_list(_) ->
  [] = books_util:to_list([]).

given_a_list_string_returns_that_string(_) ->
  "hello world" = books_util:to_list("hello world").

given_a_binary_string_returns_its_list_string_representation(_) ->
  "hello world" = books_util:to_list(<<"hello world">>).

to_list_blows_up_if_argument_not_a_string(_) ->
  teal:raises_exception(fun() -> books_util:to_list(100) end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% list_as_string tests
given_empty_list_returns_empty_string(_) ->
  "" = books_util:list_as_string([]).

given_a_list_with_one_string_returns_that_string(_) ->
  "apple" = books_util:list_as_string(["apple"]).

given_a_list_with_many_strings_returns_a_concatenation(_) ->
  "abcdef" = books_util:list_as_string(["ab", "cd", "ef"]).

given_a_list_that_contains_binary_strings_the_result_is_a_list_string(_) ->
  "abcdef" = books_util:list_as_string([<<"abc">>, "def"]).

list_as_string_blows_up_if_argument_not_a_list(_) ->
  teal:raises_exception(fun() -> books_util:list_as_string(100) end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% join tests
join_empty_list_just_returns_empty_string(_Config) ->
  "" = books_util:join("", []).

join_list_with_single_string_returns_that_string(_Config) ->
  "hello" = books_util:join(",", ["hello"]).

join_list_with_more_than_1_string_returns_those_strings_separated_by_the_separator(_Config) ->
  "hello, world" = books_util:join(", ", ["hello", "world"]).
