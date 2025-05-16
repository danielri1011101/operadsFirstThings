{# LANGUAGE DataKinds #}

-- S is a type constructor and Z is a type variable, oder? it could
-- be _lowercase z_ no problem, I think...

newtype Matrix = Matrix {unMatrix :: Vec n (Vec m a)}

data Vec n a where
  VNil :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

data Nat = Z | S Nat deriving Show

headV :: Vec (S n) a -> a
  headV (a `VCons` _) = a

data Fin n where
  FinZ :: Fin (S n)
  FinS :: Fin n -> Fin (S n)

-- With Fin n we can make this type-level Nats
-- into concrete numbers...
-- Also, Fin seems to map the positive ints into N by way
-- of _predecessor_.

ixV :: Fin n -> Vec n a -> a --     i |--> [v] |--> v_i
ixV FinZ (x `VCons` _) = x
ixV (FinS f_n)  (_ `VCons` xs) = ixV f_n xs

-- Looks all like "Type-level list comprehensions", oder?
