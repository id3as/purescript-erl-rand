module Erl.Rand
  ( Alg(..), RandState
  , bytes
  ) where

import Effect
import Erl.Data.Binary(Binary(..))
import Erl.Data.Tuple(Tuple2)
import Foreign(Foreign)

data Alg 
  = Exsss 
  | Exro928ss 
  | Exrop 
  | Exs1024s 
  | Exsp

newtype RandState = RandState Foreign

type RandResult a  = {
  result :: a
  , randState :: RandState
}

foreign import bytes :: Int -> Effect Binary

foreign import bytesS :: Int -> RandState -> Tuple2 Binary RandState
