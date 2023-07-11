module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Effect.Class (liftEffect)
import Erl.Data.Binary (byteSize)
import Erl.Data.List (List)
import Erl.Data.Tuple (fst, snd)
import Erl.Process (unsafeRunProcessM)
import Erl.Rand (Alg(..), RandState, bytes, bytesS, seed, uniformRange, uniformRangeS)
import Erl.Test.EUnit (TestF, TestSet, collectTests, suite, test)
import Test.Assert (assertEqual, assertTrue)

main_test_ :: List TestSet
main_test_ =
  collectTests randTests

randTests :: Free TestF Unit
randTests = do
  suite "RandTests" do
    test "bytes" do
      unsafeRunProcessM $ liftEffect do
        actual <- bytes 10
        assertEqual { expected: 10, actual: byteSize actual }
    test "bytesS" do
      unsafeRunProcessM $ liftEffect do
        rs :: RandState <- seed (Exsss)
        let actual = fst $ bytesS 20 rs
        assertEqual { expected: 20, actual: byteSize actual }
    test "unifromRange" do
      unsafeRunProcessM $ liftEffect do
        actual <- uniformRange  (-1) (-1) 
        assertEqual { expected: -1, actual: actual }
        actual2 <- uniformRange  10 10 
        assertEqual { expected: 10, actual: actual2 }
        actual3 <- uniformRange  (-10) 10 
        assertTrue $ actual3 >= -10 && actual3 <= 10
    test "unifromRangeS" do
      unsafeRunProcessM $ liftEffect do
        rs :: RandState <- seed (Exsss)
        let 
          r1 = uniformRangeS  (-1) (-1) rs
          r2 = uniformRangeS  10 10 $ snd r1
          r3 = uniformRangeS  (-10) 10 $ snd r2
        assertEqual { expected: -1, actual: fst r1 }
        assertEqual { expected: 10, actual: fst r2 }
        assertTrue $ fst r3 >= -10 && fst r3 <= 10
