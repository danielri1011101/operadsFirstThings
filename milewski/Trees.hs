{-# LANGUAGE DataKinds #-}

module Trees where

import Numbers


data Player = Cross | Circle deriving Eq

data Move = Move Player (Fin Three) (Fin Three)

data MoveTree n where
  Leaf :: MoveTree One
  Fan :: Trees n -> MoveTree n

data Trees n where
  NilT :: Trees Z
  (:+) :: (Move, MoveTree k) -> Trees m -> Trees (k+m)

data Forest f n m where
  Nil :: Forest f Z Z
  Cons :: f i1 -> Forest f i2 n -> Forest f (i1+i2) (S n)
