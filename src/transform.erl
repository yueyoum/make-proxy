-module(transform).

-export([transform/1]).


-define(SIGN, 2#01111001).


%% this module used for mess the data which will transfer on internet.


transform(Data) ->
    transform(Data, ?SIGN, []).


transform(<<>>, _, Res) ->
    list_to_binary( lists:reverse(Res) );

transform(<<H:8, T/binary>>, Sign, Res) ->
    transform(T, Sign, [H bxor Sign | Res]).

