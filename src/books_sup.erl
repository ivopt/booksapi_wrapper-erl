-module(books_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link(?MODULE, []).

init(_) ->
  {ok,
    {
      {one_for_one, 1, 5}, % if it crashes more than once every 5 secs, give up!
      [{
        books_repository_id,
        {books_repository, start, ["https://www.googleapis.com/books/v1/volumes?q="]},
        permanent,
        5000,
        worker,
        [books_repository]
      }]
    }
  }.