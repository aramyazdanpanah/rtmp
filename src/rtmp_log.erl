-module(rtmp_log).

-export([debug/0,
         info/0,
         error/0,
         level/0,
         level/1]).

-spec debug() -> ok.
debug() ->
    level(debug).

-spec info() -> ok.
info() ->
    level(info).

-spec error() -> ok.
error() ->
    level(error).

-spec level() -> atom().
level() ->
    lager:get_loglevel(lager_console_backend).

-spec level(atom()) -> ok.
level(Level) ->
    lager:set_loglevel(lager_console_backend, Level).
