module Erl.Rand
  ( module TypeExports
  , bytes
  , bytesS
  , normal
  , normal01
  , normal01S
  , normalS
  , seed
  , uniform
  , uniformRange
  , uniformRangeS
  , uniformS
  , uniformTo
  , uniformToS
  , updateProcessState
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.Binary (Binary)
import Erl.Rand.Unsafe as Unsafe
import Erl.Rand.Unsafe (Alg(..), RandState)
import Erl.Rand.Unsafe (Alg(..), RandState) as TypeExports
import Erl.Data.Tuple (Tuple2)
import Foreign (Foreign)

-- | Returns, for a specified integer N >= 0, a binary() with that number of random bytes
bytes :: Int -> Effect (Maybe Binary)
bytes n | n < 0 = pure Nothing
bytes n = Just <$> Unsafe.bytes n

-- | Returns, for a specified integer N >= 0 and a state, a binary() with that number of random bytes, and a new state
bytesS :: Int -> RandState -> Maybe (Tuple2 Binary RandState)
bytesS n _ | n < 0 = Nothing
bytesS n rs = Just $ Unsafe.bytesS n rs

-- | Returns a standard normal deviate float (that is, the mean is 0 and the standard deviation is 1) and updates the state in the process dictionary.
foreign import normal01 :: Effect Number
-- | Returns, for a specified state, a standard normal deviate float (that is, the mean is 0 and the standard deviation is 1) and a new state.
foreign import normal01S :: RandState -> Tuple2 Number RandState

-- | Returns a normal N(Mean, Variance) deviate float and updates the state in the process dictionary
normal ∷ Number -> Number -> Effect (Maybe Number)
normal _ v | v < (0.0) = pure Nothing
normal m v = Just <$> Unsafe.normal m v

-- | Returns, for a specified state, a standard normal deviate float (that is, the mean is 0 and the standard deviation is 1) and a new state.
normalS :: Number -> Number -> RandState -> Maybe (Tuple2 Number RandState)
normalS _ v _ | v < 0.0 = Nothing
normalS m v rs = Just $ Unsafe.normalS m v rs

foreign import seed_impl :: Atom -> Effect RandState

-- | Seeds random number generation with the specifed algorithm
seed :: Alg -> Effect RandState
seed = seed_impl <<< algToAtom

foreign import updateProcessState_impl :: RandState -> Effect Foreign

-- | Updates the process dictionary with the provided random generator state
updateProcessState :: RandState -> Effect Unit
updateProcessState rs = void $ updateProcessState_impl rs

-- | Returns, for a specified integer N >= 1, a random integer uniformly distributed in the value range 1 =< X =< N and updates the state in the process dictionary.
foreign import uniform :: Effect Number

-- | Returns, for a specified state, random float uniformly distributed in the value range 0.0 =< X < 1.0 and a new state.
foreign import uniformS :: RandState -> Tuple2 Number RandState

-- | Returns, for a specified integer N >= 1, a random integer uniformly distributed in the value range 1 =< X =< N and updates the state in the process dictionary.
uniformTo ∷ Int → Effect (Maybe Int)
uniformTo n | n < 1 = pure Nothing
uniformTo n = Just <$> Unsafe.uniformTo n

-- | Returns, for a specified integer N >= 1 and a state, a random integer uniformly distributed in the value range 1 =< X =< N and a new state.
uniformToS :: Int -> RandState -> Maybe (Tuple2 Int RandState)
uniformToS n _ | n < 1 = Nothing
uniformToS n rs = Just $ Unsafe.uniformToS n rs

-- | Generates a random integer uniformly distributed in the range provided and updates the state in the process dictionary.
uniformRange :: Int -> Int -> Effect Int
uniformRange from to | from > to = uniformRange to from
uniformRange from to = do
  rnd <- Unsafe.uniformTo $ 1 + to - from
  pure $ rnd - 1 + from

-- | Generates a random integer uniformly distributed in the range provided, along with a new state.
uniformRangeS :: Int -> Int -> RandState -> Tuple2 Int RandState
uniformRangeS from to rs | from > to = uniformRangeS to from rs
uniformRangeS from to rs =
  let
    res = Unsafe.uniformToS (1 + to - from) rs
  in
    lmap ((+) (from - 1)) res

-------------------------------------------------------------
-- Internal helpers
algToAtom :: Alg -> Atom
algToAtom Default = algToAtom Exsss
algToAtom Exsss = atom ("exsss")
algToAtom Exro928ss = atom ("exro928ss")
algToAtom Exrop = atom ("exrop")
algToAtom Exs1024s = atom ("exs1024s")
algToAtom Exsp = atom ("exsp")
