module CH2 where

import Prelude
-- import GHC.TypeLits

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True _ = 'True
  Or 'False y = y
