{-# LANGUAGE UndecidableInstances #-}
module CH2 where

import Prelude
import Data.Type.Equality
import GHC.TypeLits

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True _ = 'True
  Or 'False y = y

type family And (x :: Bool) (y :: Bool) :: Bool where
  And 'False _ = 'False
  And 'True y = y

type family Not (x :: Bool) :: Bool where
  Not 'False  = 'True
  Not 'True = 'False

type family Even (x :: Nat) :: Bool where
  Even x = (Mod x 2) == 0

type family Map (x :: a -> b) (i :: [a]) :: [b] where
  Map _ '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

-- This is not very useful because `Even` is not fully applied here
-- `Map Even '[1,2,3,4,5,6,7,8,9,10]`

---- $> :set -XDataKinds
--
---- $> import CH2
--
---- $> import Prelude
--
---- $> :kind! (Not ('True `And` 'True))
--
---- $> :kind! (Even 1000000000000000000001)
