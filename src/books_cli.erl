-module(books_cli).
-include("src/options.hrl").
-import(book_util, [join/2]).

-export([start/0, stop/0, run/1]).

start() -> application:start(books).
stop() -> application:stop(books).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Run

run([]) -> print_usage();
run(Options) ->
  SearchOptions = optparse(Options),
  BookList = books_repository:search_books(SearchOptions),
  print_books(BookList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% print_books
print_books(List) -> print_books(List, 1).

print_books([], _) -> ok;
print_books([Book| BookList], Idx) ->
  print_book(Idx, Book),
  print_books(BookList, Idx + 1).

print_book(Index, {Title, Date, Authors}) ->
  io:format("~p) Title: ~s~n", [Index, Title]),
  io:format("   Date: ~s~n", [Date]),
  io:format("   Authors: ~s~n~n", [join(", ", Authors)]).

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