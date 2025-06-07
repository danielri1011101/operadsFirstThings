{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Operads where

import Numbers
import Trees

class Graded (f :: Nat -> *) where
  grade :: f n -> SNat n

-- Idty, equivariance, and associativity are assumed but not
-- explicitly stated, right?
class (Graded f) => Operad (f :: Nat -> *) where
  ident :: f One
  comp :: f n -> Forest f m n -> f m

instance Graded MoveTree where
  grade Leaf = SS SZ
  grade (Fan ts) = grade ts

instance Graded Trees where
  grade NilT = SZ
  grade ((_, t) :+ ts) = grade t `plus` grade ts

-- Define composition "recursively", from ground-up
instance Operad MoveTree where
  ident = Leaf
  comp
