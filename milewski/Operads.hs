{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Operads where

import Numbers
import Trees
import Data.Kind
import Unsafe.Coerce

class Graded (f :: Nat -> *) where
  grade :: f n -> SNat n

-- Idty, equivariance, and associativity are assumed but not
-- explicitly stated, right?
class (Graded f) => Operad (f :: Nat -> *) where
  ident :: f One
  compose:: f n -> Forest f m n -> f m

instance Graded MoveTree where
  grade Leaf = SS SZ
  grade (Fan ts) = grade ts

instance Graded Trees where
  grade NilT = SZ
  grade ((_, t) :+ ts) = grade t `plus` grade ts

-- ConstraintKinds language extension...
-- Seems like making a type out of a constraint "by force"...
data Dict :: Constraint -> * where
  Dict :: a => Dict a

plusZ :: forall n. Dict (n ~ (n + Z))
plusZ = unsafeCoerce (Dict :: Dict (n ~ n))

-- Define composition "recursively", from ground-up
instance Operad MoveTree where
  ident = Leaf
  compose Leaf (Cons (t :: MoveTree m) Nil) =
    case plusZ :: Dict (m ~ (m + Z)) of Dict -> t
  compose (Fan ((mv, t) :+ ts)) frt =
    let (mst1, mst2) = splitForest (grade t) (grade ts) frt
        tree = compose t mst1
        (Fan trees) = compose (Fan ts) mst2
    in Fan $ (mv,t) :+ trees

-- Type signature: existential quantifier as a continuation:
-- Add base case.
splitForest :: forall m n i f r. SNat m -> SNat n -> Forest f i (m+n) ->
               (
                forall i1 i2. (i1+i2) ~ i => 
                (Forest f i1 m, Forest f i2 n) -> r
               ) -> r
splitForest SZ _ fs k = k (Nil, fs)
splitForest (SS (sm :: SNat m_1))
            sn
            (Cons (t :: f i1) (ts :: Forest f i2 (m_1+n)))
            k =
    splitForest sm sn ts $
        (
         \ ((m_frag :: Forest f i3 m_1), (n_frag :: Forest f i4 n))
            -> case plusAssoc (Proxy :: Proxy i1)
                              (Proxy :: Proxy i3)
                              (Proxy :: Proxy i4) of
                    Dict -> k (Cons t m_frag, n_frag)
        )
