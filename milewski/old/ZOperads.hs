{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module ZOperads where

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
  comp Leaf (Cons t Nil) = t
  comp (Fan ((mv, t) :+ ts)) frt =
    let (mst1, mst2) = splitForest (grade t) frt
--       mst1 :: Forest MoveTree m1 (grade t)
--       mst2 :: Forest MoveTree m2 (grade (Fan ts))
--       _where_ frt :: Forest MoveTree (m1+m2) n
--       _where_ n = (grade t) + (grade (Fan ts))
        tree = comp t mst1
        (Fan trees) = comp (Fan ts) mst2
    in Fan $ (mv,t) :+ trees

-- Wishfull thinking for the type signature:
splitForest' :: exists i1 i2. i1+i2 ~ i =>
    SNat m -> SNat n -> Forest f i (m+n) -> (Forest i1 m, Forest i2 n)

-- splitForest recall:
splitForest :: forall f m n r i. SNat m -> SNat n -> Forest f i (m+n) ->
    (
     forall i1 i2. (i1+i2) ~ i => (Forest f i1 m, Forest f i2 n) -> r
    ) -> r
splitForest SZ _ ts k = k (Nil, ts)
splitForest (SS (sm :: SNat m)) sn (Cons t ts) k =
    splitForest sm sn ts $
        (
         \ (mouts, nouts) ->
               let tree = Cons t mouts
                   
        )
