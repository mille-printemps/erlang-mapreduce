-module(word).

-export([normalize/1, get_token_list/4, make_permutation/2, make_permutation/1]).

%% Utilities for string manipulation

%% Extracts tokens in String separated by Separator,
%% then returns permutation of them

make_permutation(String, Separator) ->
    permutation(string:tokens(normalize(String), Separator), []).

make_permutation(TokenList) ->
    permutation(TokenList, []).


%% Makes pairs from Strings formatted in [String0, String1, ..., StringN]
%% [{String0, String1}, {String1, String0}, {String0, String2}, {String2, String0}, ..., {StringN, StringN-1}, {StringN-1, StringN}]

permutation([], Perms) ->
    Perms;
permutation(Strings, Perms) ->
    [String|Rest] = Strings,
    Perms1 = pair(String, Rest, Perms),
    permutation(Rest, Perms1).

pair(_String, [], Pairs) ->
    Pairs;
pair(String, Rest, Pairs) ->
    [String1|Rest1] = Rest,
    Pairs1 = [{String, String1}, {String1, String}|Pairs],
    pair(String, Rest1, Pairs1).


%% Gets N-th token when String is separated by Separator
%% This is needed because lists:nth does not treat an empty token like ",," as it is 

get_token([], _Separator, _N) ->
    {[], []};
get_token(String, _Separator, N) when N < 1 ->
    {[], String};
get_token(String, Separator, N) ->
    get_token(String, Separator, 1, N, "").

get_token([], _Separator, _I, _N, Token) ->
    {lists:reverse(Token), []};
get_token([Char|Rest], Separator, I, N, Token) ->
    case lists:member(Char, Separator) of
        true ->
            if
                I == N ->
                    {lists:reverse(Token), Rest};
                true ->
                    get_token(Rest, Separator, I+1, N, "")
            end;
        false -> get_token(Rest, Separator, I, N, [Char|Token])
    end.

%% get_token for binary

get_binary_token(<<>>, _Separator, _N) ->
    {<<>>, <<>>};
get_binary_token(Bin, _Separator, N) when N < 1 ->
    {<<>>, Bin};
get_binary_token(Bin, Separator, N) ->
     case re:compile(Separator) of
         {ok, RE} ->
             get_binary_token(Bin, RE, 1, N);
         {error, Reason} ->
             erlang:error(Reason)
     end.

get_binary_token(Bin, RE, I, N) ->
    case re:run(Bin, RE) of
        {match, [{Offset,Len}]} ->
            <<Before:Offset/binary,_:Len/binary,After/binary>> = Bin,
            if I == N ->
                    {Before, After};
               true ->
                    get_binary_token(After, RE, I+1, N)
            end;
        _ ->
            %% in case where N indicates the last element
            if I == N ->
                    {Bin, <<>>};
               true ->
                    {<<>>, Bin}
            end
    end.


%% Gets tokens at the indeces listed in Index as a list
%% Start is the first index that should be started, which is usually "1"
%% The indeces listed in Index have to be relative to Start

get_token_list(String, Separator, Indeces, TokenList) ->
    get_token_list(String, Separator, 1, Indeces, TokenList).
    
get_token_list(_String, _Separator, _Start, [], TokenList) ->
    lists:reverse(TokenList);
get_token_list(String, Separator, Start, [N|Index], TokenList) ->
    {Token, Leftover} =
        case is_binary(String) of
            true ->
                get_binary_token(String, Separator, N-Start+1);
            false ->
                get_token(String, Separator, N-Start+1)
        end,
    get_token_list(Leftover, Separator, N+1, Index, [Token|TokenList]).


%% Normalizes full-width numbers and alphabets encoded in UTF-8
%% full-width numbers -> half-width numbers
%% full-width alphabets -> half-width small alphabets

normalize(String) ->
    lists:map(fun(Char) -> normalize_char(Char) end, String).

normalize_char(Char) ->
    [[Int0]] = io_lib:format("~tc", [Char]),
    
    case character_type(Int0) of
        
        %% if the character is a full-width alphabet or full-width number,
        %% then make it a half-width small alphabet or half-width number
        full_width_number_alphabet ->
            Int1 = Int0 - 16#FEE0,
            case character_type(Int1) of
                half_width_large_alphabet ->
                    Int1 + 16#20;
                _ ->
                    Int1
            end;

        half_width_large_alphabet ->
            Int0 + 16#20;            

        %% if the characters is a full-width space,
        %% then make it a half-width space
        full_width_space ->
            16#20;

        %% nothing is done for other characters
        other ->
            Int0
    end.

character_type(Int) ->
    if
        Int == 16#3000 ->
            full_width_space;
        16#41 =< Int andalso Int =< 16#5A ->
            half_width_large_alphabet;
        16#FF10 =< Int andalso Int =< 16#FF5A ->
            full_width_number_alphabet;
        true ->
            other
    end.
