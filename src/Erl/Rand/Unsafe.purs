module Erl.Rand.Unsafe
  ( Alg(..)
  , RandState
  , bytes
  , bytesS
  , normal
  , normalS
  , uniformTo
  , uniformToS
  ) where

import Effect (Effect)
import Erl.Data.Binary (Binary)
import Erl.Data.Tuple (Tuple2)
import Foreign (Foreign)

data Alg
  = Default
  | Exsss
  | Exro928ss
  | Exrop
  | Exs1024s
  | Exsp

newtype RandState = RandState Foreign

-- | Unsafe version of bytes that crashes if provided integer N < 0
foreign import bytes :: Int -> Effect Binary

-- | Unsafe version of bytes that crashes if provided integer N < 0
foreign import bytesS :: Int -> RandState -> Tuple2 Binary RandState

-- | Unsafe version of normal that crashes if the provided variance < 0
foreign import normal :: Number -> Number -> Effect Number

-- | Unsafe version of normalS that crashes if the provided variance < 0
foreign import normalS :: Number -> Number -> RandState -> Tuple2 Number RandState

-- | Unsafe version of uniformTo that crashes if the provided integer N < 1
foreign import uniformTo :: Int -> Effect Int

-- | Unsafe version of uniformToS that crashes if the provided integer N < 1
foreign import uniformToS :: Int -> RandState -> Tuple2 Int RandState

