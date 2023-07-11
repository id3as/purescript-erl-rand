-module(erl_rand@foreign).

-export([
    bytes/1,
    bytesS/2
]).

bytes(N) ->
    fun() ->
        rand:bytes(N)
    end.

bytesS(N, State) ->
    rand:bytes_s(N, State).
