-record(book, {
  title :: binary(),
  date :: binary(),
  authors :: [binary()]
}).

-type book() :: #book{}.

-define(book(Title, Date, Authors), #book{title=Title, date=Date, authors=Authors}).
