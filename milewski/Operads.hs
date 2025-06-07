{-# LANGUAGE DataKinds #-}

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
