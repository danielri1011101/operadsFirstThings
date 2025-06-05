{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

newtype Matrix n m a = Matrix {unMatrix :: Vec n (Vec m a)}

data Vec n m where
  VNil :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

-- Thanks to the import, this can double as a kind.
data Nat = Z | S Nat deriving Show

-- Some aliases for the first few number types.

type One = S Z
type Two = S (S Z)
type Three = S (S (S Z))

-- Requires non-trivial operation of pattern matching in a
-- Generalized Abstract Data Type...
headV :: Vec (S n) a -> a
headV (VCons a _) = a

-- Type for _safe indexing_, whatever that means...
data Fin n where
  FinZ :: Fin (S n)
  FinS :: Fin n -> Fin (S n)

-- indexing function: from a coordinate i in the range [0..n[
-- and a length n vector, get the ith coefficient.
-- Aka. projection function(s).
ixV :: Fin n -> Vec n a -> a
ixV FinZ (VCons x _) = x
ixV (FinS fin_n) (VCons _ xs) = 
    ixV fin_n xs

-- Bindings for the elements of Fin Three only when the implementation
-- of the game starts.

toFin3 :: Int -> Maybe (Fin Three)
toFin3 0 = Just FinZ
toFin3 1 = Just (FinS FinZ)
toFin3 2 = Just (FinS (FinS FinZ))
toFin3 _ = Nothing

data Player = Cross | Circle deriving Eq
