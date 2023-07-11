
module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Erl.Data.Binary (byteSize)
import Erl.Data.List (List)
import Erl.Process (unsafeRunProcessM)
import Erl.Rand (bytes)
import Erl.Test.EUnit (TestF, TestSet, collectTests, suite, test, timeout)
import Test.Assert (assertEqual)

main_test_ :: List TestSet
main_test_ =
  collectTests randTests

randTests :: Free TestF Unit
randTests = do
  suite "RandTests" do
    test "bytes" do
      unsafeRunProcessM
        $ liftEffect do
            actual <- bytes 10
            assertEqual { expected: 10, actual: byteSize actual }
