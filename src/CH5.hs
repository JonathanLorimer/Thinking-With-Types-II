module CH5 where

import Prelude
import Data.Kind (Constraint, Type)

----------------------- From Book ----------------------------
data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

{- instance Eq (HList '[]) where
  HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
  (a :# as) == (b :# bs) = a == b && as == bs -}

instance All Eq ts => Eq (HList ts) where
  HNil == HNil = True
  (a :# as) == (b :# bs) = a == b && as == bs


type family All (c :: Type -> Constraint)
                (ts :: [Type]) :: Constraint where
  All _ '[]       = ()
  All c (t ': ts) = (c t, All c ts)

----------------------- Exercises ----------------------------
{- instance Ord (HList '[]) where
  HNil `compare` HNil = EQ

instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
  (a :# as) `compare` (b :# bs) =
    case a `compare` b of
      EQ -> as `compare` bs
      GT -> GT
      LT -> LT -}

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  HNil `compare` HNil = EQ
  (a :# as) `compare` (b :# bs) = a `compare` b <> as `compare` bs

{- instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
  show (a :# as) = show a <> " :# " <> show as -}

instance (All Show ts) => Show (HList ts) where
  show HNil = "HNil"
  show (a :# as) = show a <> " :# " <> show as

---- $> import CH5

---- $> show $ (1 :# 2 :# 3 :# 4 :# HNil :: HList '[Int, Int, Int, Int])
