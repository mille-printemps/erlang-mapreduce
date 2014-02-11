-module(data).

-export([for_each_line_in_file/5, files/3]).

-include_lib("kernel/include/file.hrl").

%% Utilities for reading data from files

%% Public functions

%% Does something depending on Fun for each line
%% Fun accepts Line and Args, i.e. Fun(Line, Args)

for_each_line_in_file(FileName, [_, {encoding, Encoding}]=Mode, Fun, Args, AccIn) ->
    io:setopts([{encoding, Encoding}]),
    case file:open(FileName, Mode) of
        {ok, Device} ->
            for_each_line(Device, Fun, Args, AccIn);
        {error, Reason} ->
            erlang:error(Reason)
    end.


for_each_line(Device, Fun, Args, AccIn) ->
    case io:get_line(Device, "") of
        {error, Reason} ->
            erlang:error(Reason),
            file:close(Device);
        eof ->
            file:close(Device);
        Line ->
            AccOut = Fun(Line, Args, AccIn),
            for_each_line(Device, Fun, Args, AccOut)
    end.


%% Returns a list of filenames that matches to Re on Dir
%% The implementation below is from "Programming Erlang" written by Joe Armstrong

files(Dir, Regex, Flag) ->
    case re:compile(Regex) of
        {ok, RE} ->
            case files(Dir, RE, Flag, fun(File, Acc) -> [File|Acc] end, []) of
                {ok, Files} ->
                    lists:reverse(Files);
                {error, Reason} ->
                    erlang:error(Reason)
            end;
        {error, Reason} ->
            erlang:error(Reason)
    end.


%% Private functions

files(Dir, Re, Recursive, Fun, Acc) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            find_files(Files, Dir, Re, Recursive, Fun, Acc);
        {error, Reason}  ->
            {error, Reason}
    end.


find_files([File|T], Dir, Re, Recursive, Fun, Acc0) ->
    FullName = filename:join([Dir,File]),
    case file_type(FullName) of
        regular ->
            case re:run(FullName, Re) of
                {match, _}  ->
                    Acc = Fun(FullName, Acc0),
                    find_files(T, Dir, Re, Recursive, Fun, Acc);
                _ ->
                    find_files(T, Dir, Re, Recursive, Fun, Acc0)
            end;
        directory -> 
            case Recursive of
                true ->
                    Acc1 = files(FullName, Re, Recursive, Fun, Acc0),
                    find_files(T, Dir, Re, Recursive, Fun, Acc1);
                false ->
                    find_files(T, Dir, Re, Recursive, Fun, Acc0)
            end;
        error -> 
            find_files(T, Dir, Re, Recursive, Fun, Acc0)
    end;
find_files([], _Dir, _Re, _Recursive, _Fun, A) ->
    {ok, A}.


file_type(File) ->
    case file:read_file_info(File) of
        {ok, Facts} ->
            case Facts#file_info.type of
                regular   -> regular;
                directory -> directory;
                _         -> error
            end;
        _ ->
            error
    end.


