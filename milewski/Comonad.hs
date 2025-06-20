{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

import Numbers
import LinAlg
import Operads

-- extract is the counit.
-- duplicate is the comultiplication.
class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)

-- An "on the fly" monad for _dually replicating_ the
-- Comonad definitions.
class Functor m => Moonad m where
  yoin :: m (m a) -> m a
  riturn :: a -> m a

-- Like an abstract _return_
data M f a where
  M :: f n -> Vec n a -> M f a

--instance Functor (Vec n) where
--  fmap f VNil = VNil
--  fmap f (VCons a vs) = VCons (f a) (fmap f vs)

newtype W f a = W {runW :: forall n. f n -> Vec n a}

instance Functor (Vec n) where
  fmap f VNil = VNil
  fmap f (VCons x xs) = VCons (f x) (fmap f xs)

instance Functor (W f) where
  fmap :: (a -> b) -> W f a -> W f b
  fmap g (W (k :: forall n. f n -> Vec n a)) =
    W (
       \f_n -> fmap g (k f_n)
      )

extract' :: (Operad f) => W f a -> a
extract' (W k) = case k ident of VCons a0 VNil -> a0

-- take a value and a vector, and replace the vector's coefficients with
-- the value.
replicate' :: b -> Vec n a -> Vec n b
replicate' b0 VNil = VNil
replicate' b0 (VCons a0 as) = VCons b0 (replicate' b0 as)

-- trying out a naive duplicate:
duplicate' :: W f a -> W f (W f a)
duplicate' (W k) =
  W (
     \ t_n -> replicate' (W k) (k t_n)
    )

instance Operad f => Comonad (W f) where
  extract = extract'
  duplicate = duplicate'
