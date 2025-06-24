{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module LinAlg where

import Numbers

-- This is the Linear Algebra module.

data Vec n m where
  VNil :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

vGrade :: Vec n a -> SNat n
vGrade VNil = SZ
vGrade (VCons a0 as) = SS (vGrade as)

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

-- Do a 3-way split of a vector, whose rank is a parenthezised sum.

-- Implicitly, m = n+k.
-- h_a :/ head of type a.
-- t_as :/ tail consisting of a-coefficients.
-- Attempt replacing recursive case with _case_ expression.
--  v_n_k :: Vec ((1+n)+k) a |-- t_as :: Vec (n+k) a
--
splitV :: SNat n -> SNat k -> Vec (n+k) a -> (Vec n a, Vec k a) 
splitV s_n s_k v_n_k =
    case (s_n, s_k, v_n_k) 
    of (SZ, _, v) -> (VNil, v)
       (SS s_n, s_k, (h_a `VCons` t_as)) -> (h_a `VCons` lv, rv)
         where (lv, rv) = splitV s_n s_k t_as

split3V :: SNat k -> SNat m -> SNat n -> Vec (k + (m + n)) a ->
           (Vec k a, Vec m a,Vec n a)
split3V k m n v = (vk, vm, vn)
  where
    (vm, vn) = splitV m n vmn
    (vk, vmn) = splitV k (m `plus` n) v
