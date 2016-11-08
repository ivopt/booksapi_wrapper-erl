-module(books_repository).
-behaviour(gen_server).
-include("../deps/dactyl/include/dactyl.hrl").
-include("src/options.hrl").
-include("src/book.hrl").

% Macros to assist on getting string values from maps
-define(mget(Key, Map), maps:get(list_to_binary(Key), Map)).
-define(mget(Key, Map, Default), maps:get(list_to_binary(Key), Map, Default)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Books repository API headers
-export([start/1, search_books/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GEN_SERVER API headers
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Books repository API implementation
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
-spec search_books(options()) -> [book()].
search_books(SearchTerm) ->
  gen_server:call(?MODULE, {search, SearchTerm}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server behaviour interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(State) ->
  process_flag(trap_exit, true),
  {ok, State}.

terminate(_,_) ->
  ok.

handle_call(Msg, _From, State = #{url:=Url}) ->
  case Msg of
    {search, SearchTerm} -> Response = search(SearchTerm, Url);
    _                    -> Response = nothing
  end,
  {reply, Response, State}.

% TODO - implement this...
handle_cast(_, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

code_change(_,State,_) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec search(options(), #dactyl_template{}) -> [book()].
search(#options{free_term=Term, author=Author, title=Title}, Template) ->
  % Build the URL
  Url = dactyl:render(Template, [
    {term,       encode(Term)},
    {has_author, Author =/= undefined},
    {author,     encode(Author)},
    {has_title,  Title =/= undefined},
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
-spec dactylize_url(string()) -> #dactyl_template{}.
dactylize_url(URL) ->
  TermPartial   = "~term~;",
  AuthorPartial = "~has_author~?+inauthor:~author~;~:~;",
  TitlePartial  = "~has_title~?+intitle:~title~;~:~;",
  {ok, Template} = dactyl:compile(URL ++ TermPartial ++ AuthorPartial ++ TitlePartial),
  Template.

-spec encode(undefined|string()) -> string().
encode(undefined) -> "";
encode(Term) when is_list(Term) -> http_uri:encode(Term).

-spec get_volumes(map()) -> [book()].
get_volumes(ItemMap) ->
  case ?mget("totalItems", ItemMap) of
    0 -> [];
    _ -> Items = ?mget("items", ItemMap),
         [ get_volume_details(?mget("volumeInfo",X)) || X <- Items]
  end.

-spec get_volume_details(map()) -> book().
get_volume_details(Volume) ->
  Title   = ?mget("title", Volume, ""),
  Date    = ?mget("publishedDate", Volume, ""),
  Authors = ?mget("authors", Volume, []),
  ?book(Title, Date, Authors).
