
------------------------------------------------------------------------------------------------
-- Should live in test dir, but until the spago patch is accepted, works here and not there...
------------------------------------------------------------------------------------------------
module Erl.TestHelpers 
  (checkUnsafeCrash
  )
where

import Prelude
import Effect(Effect)
foreign import checkUnsafeCrash :: forall a. (Unit -> a) -> Effect Boolean