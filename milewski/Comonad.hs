{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

import Numbers
import LinAlg

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

instance Functor (Vec n) where
  fmap f VNil = VNil
  fmap f (VCons a vs) = VCons (f a) (fmap f vs)

newtype W f a = W {runW :: forall n. f n -> Vec n a}

instance Functor (W f) where
  fmap :: (a -> b) -> W f a -> W f b
  fmap g (W (k :: forall n. f n -> Vec n a)) =
    W (
       \f_n -> fmap g (k f_n)
      )
