{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module LinAlg where

import Numbers

-- This is the Linear Algebra module.

data Vec n m where
  VNil :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

-- Matrices have two Nat number indices and a type
-- _a_ for its coefficients. More formally, they are vectors
-- of vectors.
newtype Matrix n m a = Matrix {unMatrix :: Vec n (Vec m a)}

headV :: Vec (S n) a -> a
headV (VCons x _) = x

-- Take advantage of the Numbers import to use the Fin _safe -indexing_
-- constructor to define the projection functions.
ixV :: Fin n -> Vec n a -> a
ixV FinZ (VCons a0 as) = a0
ixV (FinS fin_n) (VCons _ as) = ixV fin_n as
