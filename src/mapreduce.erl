-module(mapreduce).

-export([mapreduce/4, mapreduce/5]).

%% Public functions

%% Mapper(Pid, X) -> sends {Key,Value} messages to Pid
%% Reducer(Key, [Value], AccIn) -> AccOut

%% mapreduce for multiple hosts
%% Node is formatted in '<name>@<hostname.domain>' or '<name>@<hostname>' depending on your network environment
%% assuming that DataList that might be a list of files or entries in a database are shared with Nodes

mapreduce(Nodes, Mapper, Reducer, AccIn, DataList) ->
    Parent = self(),
    NNodes = length(Nodes),
    NData = length(DataList),
    
    if NNodes =< NData ->
            mapreduce(Nodes, Parent, Mapper, Reducer, AccIn, DataList, 1, ceil(NData/NNodes));
       true ->
            NewNodes = lists:sublist(Nodes, 1, NData),
            NNewNodes = length(NewNodes),
            mapreduce(NewNodes, Parent, Mapper, Reducer, AccIn, DataList, 1, ceil(NData/NNewNodes))
    end,
    
    receive
        {_, Result} ->
            Result
    end.


%% mapreduce for a single host
%% The implementation below borrows ideas from "Programming Erlang" written by Joe Armstrong

mapreduce(Mapper, Reducer, AccIn, DataList) ->
    Parent = self(),
    Pid = spawn(fun() -> run(Parent, Mapper, Reducer, AccIn, DataList) end),
    
    receive
        {Pid, Result} ->
            Result
    end.


%% Private functions

mapreduce([], _Parent, _Mapper, _Reducer, _AccIn, _DataList, _Start, _N) ->
    {mapreduce, ok};
mapreduce([Node|Nodes], Parent, Mapper, Reducer, AccIn, DataList, Start, N) ->
    Data = lists:sublist(DataList, Start, N),
    spawn(Node, fun() -> run(Parent, Mapper, Reducer, AccIn, Data) end),
    mapreduce(Nodes, Parent, Mapper, Reducer, AccIn, DataList, Start+N, N).


run(Parent, Mapper, Reducer, AccIn, DataList) ->
    process_flag(trap_exit, true),
    Pid = self(),
    lists:foreach(
      fun(Data) -> spawn_link(fun() -> Mapper(Pid, Data) end) end,      
      DataList
     ),
    
    N = length(DataList),
    Dict0 = dict:new(),
    Dict1 = collect(N, Dict0),
    AccOut = dict:fold(Reducer, AccIn, Dict1),
    Parent ! {self(), AccOut}.


collect(0, Dict) ->
    Dict;
collect(N, Dict) ->
    receive
        {Key, Value} ->
            case dict:is_key(Key, Dict) of
                true ->
                    Dict1 = dict:append(Key, Value, Dict),
                    collect(N, Dict1);
                false ->
                    Dict1 = dict:store(Key, [Value], Dict),
                    collect(N, Dict1)
            end;
        {'EXIT', _, _Why} ->
            collect(N-1, Dict)
    end.


ceil(X) ->
    T = trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.
