-module(erl_rand@foreign).

-export([
    bytes/1,
    bytesS/2,
    seed_impl/1,
    uniform/0,
    uniformS/1,
    uniformTo_impl/1,
    uniformToS_impl/2
]).

bytes(N) ->
    fun() ->
        rand:bytes(N)
    end.

bytesS(N, State) ->
    rand:bytes_s(N, State).

seed_impl(Alg) ->
    fun() ->
        rand:seed(Alg)
    end.

uniform() ->
    fun() ->
        rand:uniform()
    end.

uniformS(State) ->
    rand:uniform_s(State).

uniformTo_impl(N) ->
    fun() ->
        rand:uniform(N)
    end.

uniformToS_impl(N, State) ->
    rand:uniform_s(N, State).
