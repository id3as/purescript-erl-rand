module Erl.Rand
  ( Alg(..)
  , RandState
  , bytes
  , bytes'
  , bytesS
  , bytesS'
  , normal01
  , normal01S
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

foreign import bytes_impl :: Int -> Effect Binary
-- | Returns, for a specified integer N >= 0, a binary() with that number of random bytes
bytes :: Int -> Effect (Maybe Binary)
bytes n | n < 0 = pure Nothing
bytes n = Just <$> bytes_impl n

-- | Unsafe version of bytes that crashes if provided integer N < 0
bytes' ∷ Int → Effect Binary
bytes' = bytes_impl

-- | Returns, for a specified integer N >= 0 and a state, a binary() with that number of random bytes, and a new state
foreign import bytesS_impl :: Int -> RandState -> Tuple2 Binary RandState
bytesS :: Int -> RandState -> Maybe (Tuple2 Binary RandState)
bytesS n _ | n < 0 = Nothing
bytesS n rs = Just $ bytesS_impl n rs

-- | Unsafe version of bytes that crashes if provided integer N < 0
bytesS' ∷ Int → RandState ->  Tuple2 Binary RandState
bytesS' = bytesS_impl

-- | Returns a standard normal deviate float (that is, the mean is 0 and the standard deviation is 1) and updates the state in the process dictionary.
foreign import normal01 :: Effect Number
-- | Returns, for a specified state, a standard normal deviate float (that is, the mean is 0 and the standard deviation is 1) and a new state.
foreign import normal01S :: RandState -> Tuple2 Number RandState

foreign import seed_impl :: Atom -> Effect RandState

-- | Seeds random number generation with the specifed algorithm
seed :: Alg -> Effect RandState
seed = seed_impl <<< algToAtom

-- | Returns, for a specified integer N >= 1, a random integer uniformly distributed in the value range 1 =< X =< N and updates the state in the process dictionary.
foreign import uniform :: Effect Number

-- | Returns, for a specified state, random float uniformly distributed in the value range 0.0 =< X < 1.0 and a new state.
foreign import uniformS :: RandState -> Tuple2 Number RandState

foreign import uniformTo_impl :: Int -> Effect Int

-- | Returns, for a specified integer N >= 1, a random integer uniformly distributed in the value range 1 =< X =< N and updates the state in the process dictionary.
uniformTo ∷ Int → Effect (Maybe Int)
uniformTo n | n < 1 = pure Nothing
uniformTo n = Just <$> uniformTo_impl n

-- | Unsafe version of uniformTo that crashes if the provided integer N < 1
uniformTo' :: Int -> Effect Int
uniformTo' = uniformTo_impl

foreign import uniformToS_impl :: Int -> RandState -> Tuple2 Int RandState

-- | Returns, for a specified integer N >= 1 and a state, a random integer uniformly distributed in the value range 1 =< X =< N and a new state.
uniformToS :: Int -> RandState -> Maybe (Tuple2 Int RandState)
uniformToS n _ | n < 1 = Nothing
uniformToS n rs = Just $ uniformToS_impl n rs

-- | Unsafe version of uniformToS that crashes if the provided integer N < 1
uniformToS' :: Int -> RandState -> Tuple2 Int RandState
uniformToS' = uniformToS_impl

-- | Generates a random integer uniformly distributed in the range provided and updates the state in the process dictionary.
uniformRange :: Int -> Int -> Effect Int
uniformRange from to | from > to = uniformRange to from
uniformRange from to = do
  rnd <- uniformTo_impl $ 1 + to - from
  pure $ rnd - 1 + from

-- | Generates a random integer uniformly distributed in the range provided, along with a new state.
uniformRangeS :: Int -> Int -> RandState -> Tuple2 Int RandState
uniformRangeS from to rs | from > to = uniformRangeS to from rs
uniformRangeS from to rs =
  let
    res = uniformToS_impl (1 + to - from) rs
  in
    lmap ((+) (from - 1)) res

-------------------------------------------------------------
-- Internal helpers
algToAtom :: Alg -> Atom
algToAtom Exsss = atom ("exsss")
algToAtom Exro928ss = atom ("exro928ss")
algToAtom Exrop = atom ("exrop")
algToAtom Exs1024s = atom ("exs1024s")
algToAtom Exsp = atom ("exs")
