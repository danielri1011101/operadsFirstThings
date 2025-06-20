{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

newtype Matrix n m a = Matrix {unMatrix :: Vec n (Vec m a)}

-- Vec is parametrized by Nat from its definition.
data Vec n m where
  VNil :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

-- Thanks to the import, this can double as a kind.
-- .-.
data Nat = Z | S Nat deriving Show

-- Some aliases for the first few number types.
type One = S Z
type Two = S (S Z)
type Three = S (S (S Z))

-- Requires non-trivial operation of pattern matching in a
-- Generalized Abstract Data Type...  .-.
headV :: Vec (S n) a -> a
headV (VCons a _) = a

-- Type for _safe indexing_, not completely sure what that means.
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

-- For game I/O.
toFin3 :: Int -> Maybe (Fin Three)
toFin3 0 = Just FinZ
toFin3 1 = Just (FinS FinZ)
toFin3 2 = Just (FinS (FinS FinZ))
toFin3 _ = Nothing

data Player = Cross | Circle deriving Eq

instance Show Player where
  show Cross = " X "
  show Circle = " O "

-- A tic-tac-toe board is a 3x3 matrix with coefficiets "X", "O" or Nothing.
-- .-.
type Board = Matrix Three Three (Maybe Player)

-- _xc_ is "x-coordinate", _yc_ is "y-coordinate"
-- .-.
data Move = Move {plyr :: Player, xc :: Fin Three, yc :: Fin Three}

data MoveTree n where
  Leaf :: MoveTree One
  Fan :: Trees n -> MoveTree n

-- Milewski uses a pair (,) argument for the cons instead of currying... ah,
-- probably so that it can be a *binary infix operator*...   .-.
data Trees n where
  NilT :: Trees Z
  (:+) :: (Move, MoveTree k) -> Trees m -> Trees (k+m)

infixr 5 :+


-- Here's the _problematic_ type-level addition, for which Milewski says that
-- the TypeLits import can make cleaner.   .-.
type family (+) (a :: Nat) (b :: Nat) :: Nat

-- A priori *not commutative*:
type instance Z + m = m
type instance S n + m = S (n + m)

-- I remember that Homotopy Type Theory allows for commutativity to follow as
-- a theorem from this definition.  .-.

-- Singleton natural numbers.
-- Are they supposed to be types inhabited by a single element?
data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

-- Their addition.
plus :: SNat n -> SNat m -> SNat (n+m)
SZ `plus` sn_m = sn_m
(SS sn_n) `plus` sn_m = SS (sn_n `plus` sn_m)

-- Graded typeclass for Nat-parametrized types, such as operads and their trees.
class Graded (f :: Nat -> *) where
  grade :: f n -> SNat n

instance Graded MoveTree where
  grade Leaf = SS SZ
  grade (Fan ts) = grade ts

instance Graded Trees where
  grade NilT = SZ
  grade ((_, t) :+ ts) = grade t `plus` grade ts

-- A _Forest_ is a list of trees parametrized by Nat (i.e. compile-time integers.)
data Forest f n m where
  Nil :: Forest f Z Z
--   A new tree enters a forest...
  Cons :: f i1 -> Forest f i2 n -> Forest f (i1+i2) (S n)
