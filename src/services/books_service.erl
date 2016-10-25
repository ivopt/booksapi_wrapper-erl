-module(books_service).
-behaviour(gen_server).
-include("src/options.hrl").

% Macros to assist on getting string values from maps
-define(mget(Key, Map), maps:get(list_to_binary(Key), Map)).
-define(mget(Key, Map, Default), maps:get(list_to_binary(Key), Map, Default)).

% -define(pipe(Base, ListOfFunctions), ).

-compile(export_all).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Books Service API headers
% -export([start/1, search_books/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GEN_SERVER API headers
% -export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Books Service API implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Actor starting point
start(URL) ->
  gen_server:start_link(
    {local, ?MODULE},             % Server Name
    ?MODULE,                      % Module
    #{url => dactylize_url(URL)}, % Initial State
    []                            % Options
  ).

%% Search books (by free term for now...)
search_books(SearchTerm) ->
  gen_server:call(?MODULE, {search, SearchTerm}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server behaviour interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(State) ->
  hackney:start(),
  {ok, State}.

handle_call(Msg, _From, State) ->
  case Msg of
    {search, SearchTerm} ->
      Response = search(SearchTerm, maps:get(url, State));
    _ ->
      Response = nothing
  end,
  {reply, Response, State}.

handle_cast(_,_) -> todo.

handle_info(_Msg, State) ->
  io:format("Unexpected"),
  {noreply, State}.

terminate(_,_) ->
  io:format("Terminated!"),
  ok.

code_change(_,_,_) -> ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
search(#options{free_term=Term, author=Author, title=Title}, Template) ->
  % Build the URL
  Url = dactyl:render(Template, [
    {term,       encode(Term)},
    {has_author, Author =/= none},
    {author,     encode(Author)},
    {has_title,  Title =/= none},
    {title,      encode(Title)}
  ]),
  % Fetch data
  {ok, 200, _Headers, Client} = hackney:get(Url, [], "", []),
  {ok, Body} = hackney:body(Client),
  % Parse data
  ItemMap = jsone:decode(Body),
  get_volumes(ItemMap).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dactylize_url(URL) ->
  {ok, Template} = dactyl:compile(URL ++ "~term~;~has_author~?+inauthor:~author~;~:~;~has_title~?+intitle:~title~;~:~;"),
  Template.

encode(Term) when is_atom(Term) -> "";
encode(Term) when is_list(Term) -> http_uri:encode(Term).

get_volumes(ItemMap) ->
  Items = ?mget("items", ItemMap),
  [ get_volume_details(?mget("volumeInfo",X)) || X <- Items].

get_volume_details(Volume) ->
  Title   = ?mget("title", Volume),
  Date    = ?mget("publishedDate", Volume, ""),
  Authors = ?mget("authors", Volume),
  {Title, Date, Authors}.
