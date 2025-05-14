{# LANGUAGE DataKinds #}

newtype Matrix = Matrix {unMatrix :: Vec n (Vec m a)}

data Vec n a where
  VNil :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

data Nat = Z | S Nat deriving Show

headV :: Vec (S n) a -> a
  headV (VCons a _) = a

data Fin n where
  FinZ :: Fin (S n)
  FinS :: Fin n -> Fin (S n)

-- With Fin n we can make this type-level Nats
-- into concrete numbers...
