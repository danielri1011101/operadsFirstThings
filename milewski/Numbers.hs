{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Numbers where

import Data.Kind
import Unsafe.Coerce

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

-- Singleton numbers.
data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

-- Addition of singleton numbers.
plus :: SNat n -> SNat m -> SNat (n+m)
plus SZ m = m
plus (SS n) m = SS (plus n m)

-- Coercion of arithmetic properties:

---------- %%%%---------------------
---------- %%%%---------------------
--
-- Import ConstraintKinds lang. extension in Numbers module.
--
-- ConstraintKinds language extension...
-- Seems like making a type out of a constraint "by force"...
--
--

data Dict :: Constraint -> * where
  Dict :: a => Dict a

plusZ :: x n -> Dict (n ~ (n + Z))
plusZ _ = unsafeCoerce (Dict :: Dict (n ~ n))

data Proxy t = Proxy

plusAssoc :: p a -> q b -> r c -> Dict (((a+b) + c) ~ (a + (b+c)))
plusAssoc _ _ _ = unsafeCoerce (Dict :: Dict (a~a))

-- Successor associativity: 1 + (a + b) ~ (1 + a) + b
succAssoc :: p a -> q b -> Dict ((a + S b) ~ S (a + b))
succAssoc _ _ = unsafeCoerce (Dict :: Dict (a ~ a))
