-module(test_storage).

-export([test/0]).

-include("../include/macros.hrl").

%% test functions

test() ->
    io:setopts([{encoding, utf8}]),

    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(dict,
                        [
                         {disc_copies, [node()]},
                         {attributes, record_info(fields, dict)}
                        ]),    
    mnesia:stop(),

    io:format("~n"),
    
    storage:start([dict]),
    
    storage:store(dict, {?ctb(<<"ほげ">>), ?ctb(<<"ほげほげ">>)}),
    storage:store(dict, {?ctb(<<"げほ">>), ?ctb(<<"げほげほ">>)}),
    Results0 = storage:select(dict),
    
    io:format("Table : Key : Value~n"),
    lists:foreach(
      fun({Table, Key, Value}) ->  io:format("~p : ~ts : ~ts~n", [Table, ?ctl(Key), ?ctl(Value)]) end,
      Results0
     ),

    io:format("~n"),
    
    storage:reset(dict),                     
    Results2 = storage:select(dict),
    if length(Results2) == 0 ->
            io:format("the table is reset.~n");            
       true ->
            io:format("the table is not reset.~n")
    end,
    
    storage:stop().
