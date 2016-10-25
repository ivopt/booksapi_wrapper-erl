-module(books).
-export([main/1]).
-include("src/options.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main

main([]) -> print_usage();
main(Options) ->
  SearchOptions = optparse(Options),
  books_repository:start("https://www.googleapis.com/books/v1/volumes?q="),
  BookList = books_repository:search_books(SearchOptions),
  print_books(BookList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% print_books(BookList) ->
%   lists:foreach(fun print_book/1, BookList).

print_books(List) -> print_books(List, 1).
print_books([], _) -> ok;
print_books([Book| BookList], Idx) ->
  print_book(Idx, Book),
  print_books(BookList, Idx + 1).

print_book(Index, {Title, Date, Authors}) ->
  io:format("~p) Title: ~s~n", [Index, Title]),
  io:format("   Date: ~s~n", [Date]),
  io:format("   Authors: ~s~n~n", [join(", ", Authors)]).

% join/2
% @param Sep - Separator for the concatenation of all strings in List.
% @params List - List of strings to be joined.
% @return String - single string with the result of concatenating all strings and injecting Sep between them.
% join(Sep, List) -> join(Sep, List, []).
join(Sep, List) -> list_as_string(join(Sep, List, [])).

join(_Sep, [],      Acc) -> lists:reverse(Acc);
join( Sep, [A],     Acc) -> join(Sep, [], [A | Acc]);
join( Sep, [A,B|T], Acc) -> join(Sep, [B|T], [Sep, A | Acc]).

% list_as_string/1
% @param List - list of strings to be joined.
% @return String - with the result of concatenating all the strings in List.
list_as_string(List) -> list_as_string(List, "").

list_as_string([],      Acc) -> Acc;
list_as_string([A],     Acc) -> Acc ++ to_string(A);
list_as_string([A,B|T], Acc) -> list_as_string([B|T], Acc ++ to_string(A)).

to_string(A) when is_binary(A) -> binary_to_list(A);
to_string(A) when is_list(A) -> A.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% optparse
% CLI Options Parser (ruby inspired)

optparse(List) -> optparse(List, #options{}).
optparse(["-h" | _Rest], _State) -> print_usage(), halt();
optparse(["-t", Title  | Rest], State) -> optparse(Rest, State#options{title = Title});
optparse(["-a", Author | Rest], State) -> optparse(Rest, State#options{author = Author});
optparse(FreeTerms, State) -> State#options{free_term = join(" ", FreeTerms)}.

print_usage() ->
  Bin = escript:script_name(),
  io:format("Usage: ~p [options] [search term]~n"
            "~n"
            "Options:~n"
            "       -a Author~n"
            "       -t Title~n"
            "       -h shows this help screen~n"
            "~n"
            "Search Term is any string you want to search for~n"
            "~n"
            "Examples:~n"
            "  $ ~p -a Tolkien                      # search for books written by Tolkien~n"
            "  $ ~p -t Hobbit                       # search for books entitled Hobbit~n"
            "  $ ~p Hitchhiker Guide                # search for books that include the words 'Hitchhiker' or 'Guide'~n"
            "  $ ~p -a 'Douglas Adams' Hitchhiker   # search for books written by Douglas Adams and that include the word 'Hitchhiker'~n",
            [Bin,Bin,Bin,Bin,Bin]).