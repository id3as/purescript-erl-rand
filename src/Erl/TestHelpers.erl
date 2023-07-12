-module(erl_testHelpers@foreign).

-export([checkUnsafeCrash/1]).

checkUnsafeCrash(F) ->
  fun () ->
    try
      F(unit),
      false
    catch
      _:_ -> true
    end
  end.