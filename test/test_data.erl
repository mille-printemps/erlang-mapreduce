-module(test_data).

-export([print/3, test/0]).

%% Test functions

print(Line, Format, AccIn) ->
    io:format(Format, [Line]),
    AccIn.

test() ->
    {ok, Dir} = file:get_cwd(),
    [FileName] = data:files(filename:join([Dir, "data"]), "test\.csv$", false),
    data:for_each_line_in_file(FileName, [read, {encoding, utf8}], fun ?MODULE:print/3, "~ts~n", []).

