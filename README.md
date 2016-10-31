# Description

A simple CLI app that retrieves book information details from Google Books API.
The app is able to filter results by:

* author name
* book title
* free form text that can match any book detail

(any combination of these factors is allowed)

A session with the app can look like this:

```
$ ./books -a Tolkien
1) Title: OS FILHOS DE HURIN
   Date:
   Authors: J. R. R. TOLKIEN, ALAN LEE, CHRISTOPHER TOLKIEN, LUZIA APARECIDA DOS SANTOS, RONALD EDUARD KYRMSE

2) Title: O Silmarillion
   Date: 2015-08-11
   Authors: J.R.R. Tolkien

3) Title: O Hobbit
   Date:
   Authors: J.R.R. Tolkien

4) Title: Senhor Dos Aneis, O - as Duas Torres - Vol 2
   Date: 2000
   Authors: John Ronald Reuel Tolkien

5) Title: THE FELLOWSHIP OF THE RING
   Date:
   Authors: J.R.R. TOLKIEN

...
```

# Building

To build the app, be sure to have **rebar 2** laying around (I have it on my home /bin folder which is on my path).

**NOTE:** Yes, I know rebar 2 is (sort of) deprecated and people should be using rebar 3 but since I'll be
maintaining a few rebar 2 apps, I used this opportunity to learn a bit about it.

After making sure you have rebar laying around on your path, you need to install all dependencies:

```
$ rebar get-deps
==> books (get-deps)
Pulling jsone from {git,"git@github.com:sile/jsone.git"}
Cloning into 'jsone'...
Pulling dactyl from {git,"git@github.com:basho/dactyl.git"}
Cloning into 'dactyl'...
Pulling hackney from {git,"git@github.com:benoitc/hackney.git"}
Cloning into 'hackney'...

...
```

This will download all dependencies and make them available to compile.
Then all that is left is for you to compile and build the app:

```
$ rebar compile escriptize
==> jsone (compile)
Compiled src/jsone.erl
Compiled src/jsone_decode.erl
Compiled src/jsone_encode.erl
==> dactyl (compile)
Compiled src/dactyl.erl

...

==> books (escriptize)
```

This will compile all modules and dependencies and will also generate an binary escript that basically is the runnable app.

All you need to do now is to use the app!

# Usage

```
$ ./books
Usage: "./books" [options] [search term]

Options:
       -a Author
       -t Title
       -h shows this help screen

Search Term is any string you want to search for

Examples:
  $ "./books" -a Tolkien                      # search for books written by Tolkien
  $ "./books" -t Hobbit                       # search for books entitled Hobbit
  $ "./books" Hitchhiker Guide                # search for books that include the words 'Hitchhiker' or 'Guide'
  $ "./books" -a 'Douglas Adams' Hitchhiker   # search for books written by Douglas Adams and that include the word 'Hitchhiker'
```

# Development

For the test environment, there is a `rebar.config.script` file that will inject a few config details.
This works by setting the ENV variable `RUNENV=test`.

All tasks should be executed through `make` (and using the provided `Makefile`) as it accounts for the `RUNENV` detail.

## Setting up

The very first thing to do before running the tests is to install test dependencies.
To do so, run:

```
$ make test-get-deps
```

This will install all regular dependencies and also the test ones.

## Running the tests

### EUnit

To run the EUnit tests simply run:

```
$ make eunit
```

this is just a shortcut for `RUNENV=test rebar eunit`.

### Common Test

There are 2 ways to run the tests: With code coverage report or without it.
Code coverage is not the best metric ever but can be helpfull when you are refactoring tests or covering non-tested libs/apps with tests.

So, to run tests with coverage reports simply:

```
$ make ct-cover
```

to run without coverage report (which is also way faster):

```
$ make ct
```

### Running All tests

You can (and should) also run all tests at once. To do so, run:

```
$ make test-all
```

This will run both ct tests and eunit tests.

# Disclaimer

This is my very first CLI app in Erlang, I am in no manner an Erlang expert or something like that.
This is meant to be a learning exercice and a reference (either good or bad) for future work.