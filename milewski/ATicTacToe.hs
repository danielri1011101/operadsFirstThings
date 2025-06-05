{-# LANGUAGE DataKinds #-}

newtype Matrix n m a = Matrix {unMatrix :: Vec n (Vec m a)}

data Vec n m where
  VNil :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

data Nat = Z | S Nat
