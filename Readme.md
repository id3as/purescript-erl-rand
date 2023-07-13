Wrapper round the erlang rand module. Most methods have been implemented (`rand:jump` being an exception) - PRs welcome!

It also provides `uniformRange` and `uniformRangeS` methods, peculiarly missing from the Erlang library.

Some of the methods can crash in Erlang if given invalid ranges (e.g. `rand:uniform(-1)` as `rand:uniform(N)` returns a random integer between 1 and N).  That's why many of the exposed methods (such as `uniformTo`) come in two flavours - one that returns a `Maybe` (if the input is invalid) and an unsafe version in `Erl.Rand.Unsafe` that raises a runtime error if the input is invalid.  In practice, user code is likely to be full of `unsafeFromJust` calls, as it is normally a programming error if the data is wrong, so the `Unsafe` module provides versions to avoid the clutter...

Note the uniformRange methods do not suffer from this, so `uniformRange (-10) 10` and `uniformRange 10 (-10)` are both valid and equivalent.
