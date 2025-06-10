{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module YOperads where

import Numbers
import YTrees
import Data.Kind
import Unsafe.Coerce

class Graded (f :: Nat -> *) where
  grade :: f n -> SNat n

type Preforest = [YTrees]

-- Idty, equivariance, and associativity are assumed but not
-- explicitly stated, right?
class (Graded f) => YOperad (f :: Nat -> *) where
  ident :: f One
  compose:: f n -> Forest f m n -> f m


