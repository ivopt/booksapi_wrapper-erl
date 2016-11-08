-record(options, {
  title :: string(),
  author :: string(),
  free_term :: string()
}).

-type options() :: #options{}.