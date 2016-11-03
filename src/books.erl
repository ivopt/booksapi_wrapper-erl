-module(books).
-behaviour(application).
-behaviour(supervisor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OTP application behaviour API
-export([start/2, stop/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OTP supervisor behaviour API
-export([start_link/0]).
-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Client application API
-export([start/0, stop/0, run_cli/1, search/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Escriptize main function
-export([main/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OTP application behaviour API implementation
start(_Type, _Args) -> start_link().
stop(_) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OTP supervisor behaviour API implementation
start_link() -> supervisor:start_link(?MODULE, []).

init(_) ->
  {ok, {
    {one_for_one, 1, 5},
    [
      #{ id    => books_repository_id,
         start => {books_repository, start, ["https://www.googleapis.com/books/v1/volumes?q="]} }
    ]
  }}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Client application API implementation
start() -> application:start(books).
stop()  -> application:stop(books).

run_cli(Args)   -> books_cli:run(Args, {?MODULE}).
search(Options) -> books_repository:search_books(Options).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Escriptize main function implementation
main(Args) ->
  start(),
  Response = run_cli(Args),
  io:format(Response),
  stop().
