module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..), isNothing)
import Debug (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (catchException)
import Effect.Unsafe (unsafePerformEffect)
import Erl.Data.Binary (byteSize)
import Erl.Data.List (List)
import Erl.Data.Tuple (fst, snd)
import Erl.Process (unsafeRunProcessM)
import Erl.Rand (Alg(..), RandState, bytes, bytesS, seed, uniform, uniformRange, uniformRangeS, uniformS, uniformTo, uniformTo', uniformToS, uniformToS')
import Erl.Test.EUnit (TestF, TestSet, collectTests, suite, test)
import Partial.Unsafe (unsafeCrashWith)
import Test.Assert (assertEqual, assertThrows, assertTrue)

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
    test "uniform" do
        r1 <- uniform 
        r2 <- uniform 
        r3 <- uniform 
        assertTrue $ r1 >= 0.0 && r1 <= 1.0
        assertTrue $ r2 >= 0.0 && r2 <= 1.0
        assertTrue $ r3 >= 0.0 && r3 <= 1.0
    test "uniformS" do
      unsafeRunProcessM $ liftEffect do
        rs :: RandState <- seed (Exsss)
        let 
          r1 = spy "r1" $ uniformS  rs
          r2 = uniformS  $ snd r1
          r3 = uniformS  $ snd r2
        assertTrue $ fst r1 >= 0.0 && fst r1 <= 1.0
        assertTrue $ fst r2 >= 0.0 && fst r2 <= 1.0
        assertTrue $ fst r3 >= 0.0 && fst r3 <= 1.0
    test "uniformRange" do
      unsafeRunProcessM $ liftEffect do
        actual1 <- uniformRange  (-1) (-1) 
        assertEqual { expected: -1, actual: actual1 }
        actual2 <- uniformRange  10 10 
        assertEqual { expected: 10, actual: actual2 }
        actual3 <- uniformRange  (-10) 10 
        assertTrue $ actual3 >= -10 && actual3 <= 10
        -- Range requests don't have to be low to high
        actual4 <- uniformRange  1  (-1) 
        assertTrue $ actual4 >= -1 && actual4 <= 1
    test "uniformRangeS" do
      unsafeRunProcessM $ liftEffect do
        rs :: RandState <- seed (Exsss)
        let 
          r1 = uniformRangeS  (-1) (-1) rs
          r2 = uniformRangeS  10 10 $ snd r1
          r3 = uniformRangeS  (-10) 10 $ snd r2
          r4 = uniformRangeS  (-1) 1 $ snd r3
        assertEqual { expected: -1, actual: fst r1 }
        assertEqual { expected: 10, actual: fst r2 }
        assertTrue $ fst r3 >= -10 && fst r3 <= 10
        -- Range requests don't have to be low to high
        assertTrue $ fst r4 >= -1 && fst r4 <= 1
    test "uniformTo" do
      unsafeRunProcessM $ liftEffect do
        actual1 <- unsafeFromJust "Range is valid" <$> uniformTo  1
        assertEqual { expected: 1, actual: actual1 }
        actual2 <- unsafeFromJust "Range is valid" <$> uniformTo  2
        assertTrue $ actual2 >= 1 && actual2 <= 2
        actual3 <- uniformTo  0
        assertTrue $ isNothing actual3
    test "uniformToS" do
      unsafeRunProcessM $ liftEffect do
        rs :: RandState <- seed (Exsss)
        let 
          r1 = unsafeFromJust "Range is valid" $ uniformToS 1 rs
          r2 = unsafeFromJust "Range is valid" $ uniformToS 2 $ snd r1
          r3 = uniformToS 0 $ snd r2
        assertEqual { expected: 1, actual: fst r1 }
        assertTrue $ fst r2 >= 1 && fst r2 <= 2
        assertTrue $ isNothing r3
    test "uniformTo'" do
      unsafeRunProcessM $ liftEffect do
        actual1 <- uniformTo'  1
        assertEqual { expected: 1, actual: actual1 }
        actual2 <- uniformTo'  2
        assertTrue $ actual2 >= 1 && actual2 <= 2
        crashed <- checkCrashes $ uniformTo' 0
        assertTrue crashed
    test "uniformToS'" do
      unsafeRunProcessM $ liftEffect do
        rs :: RandState <- seed (Exsss)
        let 
          r1 = uniformToS' 1 rs
          r2 = uniformToS' 2 $ snd r1
          -- uniformToSEffect :: Effect _ --(Tuple2 Int RandState)
          -- uniformToSEffect = pure $ uniformToS' 0 $ snd r2
          
        assertEqual $ spy "assert"{ expected: 1, actual: fst r1 }
        assertTrue $ fst r2 >= 1 && fst r2 <= 2
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

foreign import checkUnsafeCrash :: forall a. (Unit -> a) -> Effect Boolean
