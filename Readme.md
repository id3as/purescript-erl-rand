Wrapper round the erlang rand module. Most methods have been implemented (`rand:jump` being an exception) - PRs welcome!

It also provides `uniformRange` and `uniformRangeS` methods, peculiarly missing from the Erlang library.

Some of the methods can crash in Erlang if given invalid ranges (e.g. `rand:uniform(-1)` as `rand:uniform(N)` returns a random integer between 1 and N).  That's why many of the exposed methods (such as `uniformTo`) come in two flavours - one that return a `Maybe` (if the input is invalid) and an unsafe prime(') version (e.g. `uniformTo'`) that raises a runtime error if the input is invalid.  In practice, user code is likely to be full of `unsafefromJust` as it is likely a programming error if the data is wrong, so the wrapper provides unsafe version to avoid the clutter...

Note the uniformRange methods do not suffer from this ( so `uniformRange (-10) 10` and `uniformRange 10 (-10)` are both acceptable).
