{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Numbers where

data Nat = Z | S Nat

type One = S Z
type Two = S One
type Three = S Two

-- For _safe indexing_.
data Fin n where
  FinZ :: Fin (S n)
  FinS :: Fin n -> Fin (S n)

type family (+) (a :: Nat) (b :: Nat) :: Nat

-- Def. of non-commutative addition.
type instance Z + m = m
type instance S n + m = S (n+m)
