-module(books).
-export([main/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main
main(Args) ->
  books_cli:start(),
  books_cli:run(Args),
  books_cli:stop().
