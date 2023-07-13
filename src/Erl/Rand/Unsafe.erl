-module(erl_rand_unsafe@foreign).

-export([
    bytes/1,
    bytesS/2,
    normal/2,
    normalS/3,
    uniformTo/1,
    uniformToS/2
]).

bytes(N) ->
    fun() ->
        rand:bytes(N)
    end.

bytesS(N, State) ->
    rand:bytes_s(N, State).

normal(Mean, Variance) ->
    fun() ->
        rand:normal(Mean, Variance)
    end.

normalS(Mean, Variance, State) ->
    rand:normal_s(Mean, Variance, State).

uniformTo(N) ->
    fun() ->
        rand:uniform(N)
    end.

uniformToS(N, State) ->
    rand:uniform_s(N, State).
