module CH3 where

import Prelude
import Data.Functor.Contravariant

newtype T1 a = T1 (Int -> a)

instance Functor T1 where
  fmap f (T1 g) = T1 $ f . g

newtype T2 a = T2 (a -> Int) -- Not a functor, is Contravariant

instance Contravariant T2 where
  contramap f (T2 g) = T2 $ g . f

newtype T3 a = T3 (a -> a) -- Not a functor, is Invariant

class Invariant f where
  invmap :: (a -> b) -> (b -> a) -> f a -> f b

instance Invariant T3 where
  invmap ab ba (T3 f) = T3 $ ab . f . ba

newtype T4 a = T4 ((Int -> a) -> Int) -- Not a functor, is Contravariant

instance Contravariant T4 where
  contramap f (T4 g) = T4 $ \iToB -> g (f . iToB)

newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T5 where
  fmap f (T5 g) = T5 \aToInt -> g $ aToInt . f
