-module(test_word).

-export([make_permutation/3, test/0, string_split/1, binary_split/1, time/1]).

-include("../include/macros.hrl").

-define(test_data, "ほげ,ほげほげ,ふが,ふがふが,げげ,げげげげ,ごご,ごごごご,,ほげふが,ほげふがほげふが,ふがげげ,ふがげげふがげげ,").


%% Test functions

get_string_token_list(_Arg) ->
    word:get_token_list(?ctl(<<?test_data>>), ",", [3,4,5,6,10], []).    

get_binary_token_list(_Arg) ->
    word:get_token_list(?ctb(?test_data), ",", [3,4,5,6,10], []).

string_split(Iter) ->
    split(fun get_string_token_list/1, [], Iter).

binary_split(Iter) ->
    split(fun get_binary_token_list/1, [], Iter).

split(F, Arg, Iter) ->
    if Iter /= 0 ->
            F(Arg),
            split(F, Arg, Iter-1);
    true ->
            ok
    end.

time(Iter) ->
   {Time1, R1} = timer:tc(?MODULE, string_split, [Iter]),
    io:format("string split: ~p~n", [{Time1, R1}]),
    
    {Time2, R2} = timer:tc(?MODULE, binary_split, [Iter]),
    io:format("binary split: ~p~n", [{Time2, R2}]).


test_get_token() ->
    io:setopts([{encoding, utf8}]),

    TokenList = word:get_token_list(?ctl(<<?test_data>>), ",", [3,4,5,6,10], []),
    lists:foreach(
      fun(Word) -> io:format("~ts~n", [Word]) end,
      TokenList
     ),
    
    io:format("~n"),
    
    BinaryTokenList = word:get_token_list(?ctb(<<?test_data>>), ",", [3,4,5,6,10], []),
    lists:foreach(
      fun(Word) -> io:format("~ts~n", [Word]) end,
      BinaryTokenList
    ).

test_make_permutation() ->
    {ok, Dir} = file:get_cwd(),
    [Filename] = data:files(filename:join([Dir, "data"]), "test\.csv$", false),
    data:for_each_line_in_file(Filename, [read, {encoding, utf8}], fun ?MODULE:make_permutation/3, [10,11,12,13], []).

make_permutation(Line, N, AccIn) ->
    TokenList = word:get_token_list(Line, ",", N, []),
    Perms = word:make_permutation(TokenList),    
    lists:foreach(
      fun({Key, Val}) -> io:format("~ts,~ts~n", [Key, Val]) end,
      Perms
     ),
    AccIn.


test() ->
    test_get_token(),
    io:format("~n"),
    test_make_permutation().
