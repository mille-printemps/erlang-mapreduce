-module(storage).

-export([start/1, stop/0, reset/1, select/1, store/2, do/1]).

-include_lib("stdlib/include/qlc.hrl").

start(Tables) ->
    mnesia:start(),
    mnesia:wait_for_tables(Tables, 20000).

stop() ->
    mnesia:stop().

reset(Table) ->
    mnesia:clear_table(Table).

select(Table) ->
    do(qlc:q([Row || Row <- mnesia:table(Table)])).

store(Table, {Key, Value}) ->
    Row = {Table, Key, Value},    
    mnesia:transaction(fun() -> mnesia:write(Row) end).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Results} = mnesia:transaction(F),
    Results.
