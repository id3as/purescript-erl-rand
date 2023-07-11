module Erl.Rand
  ( Alg(..)
  , RandState
  , bytes
  , bytesS
  , seed
  , uniform
  , uniformRange
  , uniformRangeS
  , uniformS
  , uniformTo
  , uniformTo'
  , uniformToS
  , uniformToS'
  )
  where

import Prelude

import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.Binary (Binary)
import Erl.Data.Tuple (Tuple2)
import Foreign (Foreign)

data Alg
  = Exsss
  | Exro928ss
  | Exrop
  | Exs1024s
  | Exsp

newtype RandState = RandState Foreign

-- | Returns, for a specified integer N >= 0, a binary() with that number of random bytes
foreign import bytes :: Int -> Effect Binary
-- | Returns, for a specified integer N >= 0 and a state, a binary() with that number of random bytes, and a new state
foreign import bytesS :: Int -> RandState -> Tuple2 Binary RandState

foreign import seed_impl :: Atom -> Effect RandState

-- | Seeds random number generation with the specifed algorithm
seed :: Alg -> Effect RandState
seed = seed_impl <<< algToAtom

-- | Returns, for a specified integer N >= 1, a random integer uniformly distributed in the value range 1 =< X =< N and updates the state in the process dictionary.
foreign import uniform :: Effect Number

-- | Returns, for a specified state, random float uniformly distributed in the value range 0.0 =< X < 1.0 and a new state.
foreign import uniformS :: RandState -> Tuple2 Int RandState

foreign import uniformTo_impl :: Int -> Effect Int

uniformTo ∷ Int → Effect (Maybe Int)
uniformTo n | n < 1 = pure Nothing
uniformTo n = Just <$> uniformTo_impl n

uniformTo' :: Int -> Effect Int
uniformTo' = uniformTo_impl

foreign import uniformToS_impl :: Int -> RandState -> Tuple2 Int RandState

uniformToS :: Int -> RandState -> Maybe (Tuple2 Int RandState)
uniformToS n _ | n < 1 = Nothing
uniformToS n rs = Just $ uniformToS_impl n rs

uniformToS' :: Int -> RandState -> Tuple2 Int RandState
uniformToS' = uniformToS_impl

uniformRange :: Int -> Int -> Effect Int
uniformRange from to | from > to = uniformRange to from
uniformRange from to = do
  rnd <- uniformTo_impl $ 1 + to - from
  pure $ rnd - 1 + from

uniformRangeS :: Int -> Int -> RandState -> Tuple2 Int RandState
uniformRangeS from to rs | from > to = uniformRangeS to from rs
uniformRangeS from to rs =
  let
    res = uniformToS_impl (1 + to - from) rs
  in
    lmap ((+) (from - 1)) res

-- | Returns, for a specified integer N >= 1 and a state, a random integer uniformly distributed in the value range 1 =< X =< N and a new state.
-- uniformRangeS :: Int -> Int -> RandState -> Maybe (Tuple2 Int RandState)
-- uniformRangeS from to rs = 
--   case uniformToS 

-- uniform/0, uniform/1, uniform_s/1, uniform_s/2,
-- uniform_real/0, uniform_real_s/1,
-- jump/0, jump/1,
-- normal/0, normal/2, normal_s/1, normal_s/3

-------------------------------------------------------------
-- Internal helpers
algToAtom :: Alg -> Atom
algToAtom Exsss = atom ("exsss")
algToAtom Exro928ss = atom ("exro928ss")
algToAtom Exrop = atom ("exrop")
algToAtom Exs1024s = atom ("exs1024s")
algToAtom Exsp = atom ("exs")
