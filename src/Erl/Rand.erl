-module(erl_rand@foreign).

-export([
    normal01/0,
    normal01S/1,
    seed_impl/1,
    updateProcessState_impl/1,
    uniform/0,
    uniformS/1
]).

normal01() ->
    fun() ->
        rand:normal()
    end.

normal01S(State) ->
    rand:normal_s(State).

seed_impl(Alg) ->
    fun() ->
        rand:seed(Alg)
    end.

updateProcessState_impl(State) ->
    fun() ->
        rand:seed(State)
    end.

uniform() ->
    fun() ->
        rand:uniform()
    end.

uniformS(State) ->
    rand:uniform_s(State).
