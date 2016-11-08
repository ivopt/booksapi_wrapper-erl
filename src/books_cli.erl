-module(books_cli).
-include("src/options.hrl").
-include("src/book.hrl").
-import(books_util, [join/2]).

-export([run/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Run
-spec run([string()], tuple()) -> string().
run([], _) -> print_usage();
run(Options, {App}) ->
  case optparse(Options) of
    {halt, HelpText} ->
      HelpText;
    {search, SearchOptions = #options{}} ->
      BookList = App:search(SearchOptions),
      print_books(BookList)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% print_books
-spec print_books([book()]) -> string().
print_books(List) -> print_books(List, 1).

-spec print_books([book()], number()) -> string().
print_books([], _) -> "Your search criteria does match any book~n";
print_books(BookList, Idx) ->
  {PrintedList, _} = lists:mapfoldl(fun fold_print_book/2, Idx, BookList),
  join("~n", PrintedList).

fold_print_book(Book, Idx) -> { print_book(Book, Idx), Idx + 1 }.

-spec print_book(book(), number()) -> string().
print_book(#book{title=Title, date=Date, authors=Authors}, Index) ->
  io_lib:format("~2w) Title: ~s~n"
                "    Date: ~s~n"
                "    Authors: ~s~n", [Index, Title, Date, join(", ", Authors)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% optparse
% CLI Options Parser (ruby inspired)
-spec optparse([string()]) -> {search, options()}
                            | {halt, string()}.
optparse(List) -> optparse(List, #options{}).

optparse(["-h" | _Rest], _State) -> {halt, print_usage()};
optparse(["-t", Title  | Rest], State) -> optparse(Rest, State#options{title = Title});
optparse(["-a", Author | Rest], State) -> optparse(Rest, State#options{author = Author});
optparse(FreeTerms, State) -> {search, State#options{free_term = join(" ", FreeTerms)} }.

print_usage() ->
  Bin = escript:script_name(),
  io_lib:format("Usage: ~s [options] [search term]~n"
                "~n"
                "Options:~n"
                "       -a Author~n"
                "       -t Title~n"
                "       -h shows this help screen~n"
                "~n"
                "Search Term is any string you want to search for~n"
                "~n"
                "Examples:~n"
                "  $ ~s -a Tolkien                      # search for books written by Tolkien~n"
                "  $ ~s -t Hobbit                       # search for books entitled Hobbit~n"
                "  $ ~s Hitchhiker Guide                # search for books that include the words 'Hitchhiker' or 'Guide'~n"
                "  $ ~s -a 'Douglas Adams' Hitchhiker   # search for books written by Douglas Adams and that include the word 'Hitchhiker'~n",
                [Bin,Bin,Bin,Bin,Bin]).