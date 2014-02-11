-define(ctb(U), unicode:characters_to_binary((U))).
-define(ctl(U), unicode:characters_to_list((U))).

-record(sequence, {key, index}).
-record(dict, {key, value}).


