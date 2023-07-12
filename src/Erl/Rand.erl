-module(erl_rand@foreign).

-export([
    bytes_impl/1,
    bytesS_impl/2,
    normal01/0,
    normal01S/1,
    normal_impl/2,
    normalS_impl/3,
    seed_impl/1,
    uniform/0,
    uniformS/1,
    uniformTo_impl/1,
    uniformToS_impl/2
]).

bytes_impl(N) ->
    fun() ->
        rand:bytes(N)
    end.

bytesS_impl(N, State) ->
    rand:bytes_s(N, State).

normal01() ->
    fun() ->
        rand:normal()
    end.

normal01S(State) ->
    rand:normal_s(State).

normal_impl(Mean, Variance) ->
    fun() ->
        rand:normal(Mean, Variance)
    end.

normalS_impl(Mean, Variance, State) ->
    rand:normal_s(Mean, Variance, State).

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
