module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..), isNothing)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (catchException)
import Erl.Data.Binary (byteSize)
import Erl.Data.List (List)
import Erl.Data.Tuple (fst, snd)
import Erl.Process (unsafeRunProcessM)
import Erl.Rand (Alg(..), RandState, bytes, bytes', bytesS, bytesS', normal, normal', normal01, normal01S, normalS, normalS', seed, uniform, uniformRange, uniformRangeS, uniformS, uniformTo, uniformTo', uniformToS, uniformToS', updateProcessState)
import Erl.Test.EUnit (TestF, TestSet, collectTests, suite, test)
import Erl.TestHelpers (checkUnsafeCrash)
import Partial.Unsafe (unsafeCrashWith)
import Test.Assert (assertEqual, assertTrue)

main_test_ :: List TestSet
main_test_ =
  collectTests randTests

randTests :: Free TestF Unit
randTests = do
  suite "RandTests" do
    test "bytes" do
      unsafeRunProcessM $ liftEffect do
        actual <- unsafeFromJust "Range is valid" <$> bytes 10
        assertEqual { expected: 10, actual: byteSize actual }
        actual2 <- bytes (-1)
        assertEqual { expected: Nothing, actual: actual2 }
    test "bytesS" do
      unsafeRunProcessM $ liftEffect do
        rs :: RandState <- seed Exsss
        let
          actual1 = unsafeFromJust "Range is valid" $ bytesS 20 rs
          actual2 = bytesS (-1) $ snd actual1
        assertEqual { expected: 20, actual: byteSize $ fst actual1 }
        assertTrue $ isNothing actual2
    test "bytes'" do
      unsafeRunProcessM $ liftEffect do
        actual1 <- bytes' 5
        assertEqual { expected: 5, actual: byteSize actual1 }
        actual2 <- bytes' 0
        assertEqual { expected: 0, actual: byteSize actual2 }
        crashed <- checkCrashes $ bytes' (-1)
        assertTrue crashed
    test "bytesS'" do
      unsafeRunProcessM $ liftEffect do
        rs :: RandState <- seed Exsss
        let
          r1 = bytesS' 1 rs
          r2 = bytesS' 0 $ snd r1
        assertEqual { expected: 1, actual: byteSize $ fst r1 }
        assertEqual { expected: 0, actual: byteSize $ fst r2 }
        crashed <- checkUnsafeCrash (\_ -> bytesS' (-1) $ snd r2)
        assertTrue crashed
    test "normal01" do
      r1 <- normal01
      r2 <- normal01
      r3 <- normal01
      assertBetween (-100.0) 100.0 r1
      assertBetween (-100.0) 100.0 r2
      assertBetween (-100.0) 100.0 r3
    test "normal01S" do
      unsafeRunProcessM $ liftEffect do
        rs :: RandState <- seed Exsss
        let
          r1 = normal01S rs
          r2 = normal01S $ snd r1
          r3 = normal01S $ snd r2
        assertBetween (-100.0) 100.0 $ fst r1
        assertBetween (-100.0) 100.0 $ fst r2
        assertBetween (-100.0) 100.0 $ fst r3
    test "normal" do
      r1 <- unsafeFromJust "Range is valid" <$> normal 100.0 1.0
      r2 <- unsafeFromJust "Range is valid" <$> normal (-100.0) 1.0
      r3 <- unsafeFromJust "Range is valid" <$> normal 0.0 10.0
      r4 <- normal 0.0 (-0.0001)
      assertBetween (0.0) 200.0 r1
      assertBetween (-200.0) 0.0 r2
      assertBetween (-100.0) 100.0 r3
      assertTrue $ isNothing r4
    test "normalS" do
      unsafeRunProcessM $ liftEffect do
        rs :: RandState <- seed Exsss
        let
          r1 = unsafeFromJust "Range is valid" $ normalS 100.0 1.0 rs
          r2 = unsafeFromJust "Range is valid" $ normalS (-100.0) 1.0 $ snd r1
          r3 = unsafeFromJust "Range is valid" $ normalS 0.0 10.0 $ snd r2
          r4 = normalS 0.0 (-0.0001) $ snd r3
        assertBetween (0.0) 200.0 $ fst r1
        assertBetween (-200.0) 0.0 $ fst r2
        assertBetween (-100.0) 100.0 $ fst r3
        assertTrue $ isNothing r4
    test "normal'" do
      r1 <- normal' 100.0 1.0
      r2 <- normal' (-100.0) 1.0
      r3 <- normal' 0.0 10.0
      crashed <- checkCrashes $ normal' 0.0 (-0.0001)
      assertBetween (0.0) 200.0 r1
      assertBetween (-200.0) 0.0 r2
      assertBetween (-100.0) 100.0 r3
      assertTrue $ crashed
    test "normalS'" do
      unsafeRunProcessM $ liftEffect do
        rs :: RandState <- seed Exsss
        let
          r1 = normalS' 100.0 1.0 rs
          r2 = normalS' (-100.0) 1.0 $ snd r1
          r3 = normalS' 0.0 10.0 $ snd r2
        crashed <- checkUnsafeCrash (\_ -> normalS' 0.0 (-0.0001) $ snd r3)
        assertBetween (0.0) 200.0 $ fst r1
        assertBetween (-200.0) 0.0 $ fst r2
        assertBetween (-100.0) 100.0 $ fst r3
        assertTrue crashed
    test "seed and updateProcessState" do
      unsafeRunProcessM $ liftEffect do
        let 
          exerciseAlg alg = do
            rs <- seed alg
            updateProcessState rs
            r1 <- uniform
            r2 <- uniform
            updateProcessState rs
            r3 <- uniform
            assertBetween 0.0 1.0 r1
            assertBetween 0.0 1.0 r2
            assertTrue $ r1 /= r2
            assertEqual {expected: r1, actual: r3}

        exerciseAlg Exsss
        exerciseAlg Exro928ss
        exerciseAlg Exrop
        exerciseAlg Exs1024s
        exerciseAlg Exsp
    test "uniform" do
      r1 <- uniform
      r2 <- uniform
      r3 <- uniform
      assertBetween 0.0 1.0 r1
      assertBetween 0.0 1.0 r2
      assertBetween 0.0 1.0 r3
    test "uniformS" do
      unsafeRunProcessM $ liftEffect do
        rs :: RandState <- seed Exsss
        let
          r1 = uniformS rs
          r2 = uniformS $ snd r1
          r3 = uniformS $ snd r2
        assertBetween 0.0 1.0 $ fst r1
        assertBetween 0.0 1.0 $ fst r2
        assertBetween 0.0 1.0 $ fst r3
    test "uniformRange" do
      unsafeRunProcessM $ liftEffect do
        actual1 <- uniformRange (-1) (-1)
        assertEqual { expected: -1, actual: actual1 }
        actual2 <- uniformRange 10 10
        assertEqual { expected: 10, actual: actual2 }
        actual3 <- uniformRange (-10) 10
        assertBetween (-10) 10 actual3
        -- Range requests don't have to be low to high
        actual4 <- uniformRange 1 (-1)
        assertBetween (-1) 1 actual4
    test "uniformRangeS" do
      unsafeRunProcessM $ liftEffect do
        rs :: RandState <- seed Exsss
        let
          r1 = uniformRangeS (-1) (-1) rs
          r2 = uniformRangeS 10 10 $ snd r1
          r3 = uniformRangeS (-10) 10 $ snd r2
          r4 = uniformRangeS (-1) 1 $ snd r3
        assertEqual { expected: -1, actual: fst r1 }
        assertEqual { expected: 10, actual: fst r2 }
        assertBetween (-10) 10 $ fst r3
        -- Range requests don't have to be low to high
        assertBetween (-1) 1 $ fst r4
    test "uniformTo" do
      unsafeRunProcessM $ liftEffect do
        actual1 <- unsafeFromJust "Range is valid" <$> uniformTo 1
        assertEqual { expected: 1, actual: actual1 }
        actual2 <- unsafeFromJust "Range is valid" <$> uniformTo 2
        assertBetween 1 2 actual2
        actual3 <- uniformTo 0
        assertTrue $ isNothing actual3
    test "uniformToS" do
      unsafeRunProcessM $ liftEffect do
        rs :: RandState <- seed Exsss
        let
          r1 = unsafeFromJust "Range is valid" $ uniformToS 1 rs
          r2 = unsafeFromJust "Range is valid" $ uniformToS 2 $ snd r1
          r3 = uniformToS 0 $ snd r2
        assertEqual { expected: 1, actual: fst r1 }
        assertBetween 1 2 $ fst r2
        assertTrue $ isNothing r3
    test "uniformTo'" do
      unsafeRunProcessM $ liftEffect do
        actual1 <- uniformTo' 1
        assertEqual { expected: 1, actual: actual1 }
        actual2 <- uniformTo' 2
        assertBetween 1 2 actual2
        crashed <- checkCrashes $ uniformTo' 0
        assertTrue crashed
    test "uniformToS'" do
      unsafeRunProcessM $ liftEffect do
        rs :: RandState <- seed Exsss
        let
          r1 = uniformToS' 1 rs
          r2 = uniformToS' 2 $ snd r1
        assertEqual { expected: 1, actual: fst r1 }
        assertBetween 1 2 $ fst r2
        crashed <- checkUnsafeCrash (\_ -> uniformToS' 0 $ snd r2)
        assertTrue crashed

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------
unsafeFromJust :: forall a. String -> Maybe a -> a
unsafeFromJust _ (Just a) = a
unsafeFromJust message Nothing = unsafeCrashWith message

checkCrashes :: forall a. Effect a -> Effect Boolean
checkCrashes f =
  catchException (\_ -> pure true) $ (\_ -> false) <$> f

assertBetween ∷ ∀ a. Ord a ⇒ a → a → a → Effect Unit
assertBetween low high val =
  assertTrue $ val >= low && val <= high