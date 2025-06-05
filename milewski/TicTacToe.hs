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

type One = S Z
type Two = S One 
type Three = S Two

-- Binding the elements of Fin Three:

let fzero = FinZ :: Fin Three
    fone = FinS FinZ :: Fin Three
    ftwo = FinS (FinS FinZ) :: Fin Three

-- Defining the move-trees for ticTacToe:

data Trees n where
  NilT :: Trees Z
  Fan :: Trees n -> MoveTree n

data MoveTree n where
  Leaf :: MoveTree One
  (:+) :: (Move, MoveTree k) -> Trees m -> Trees (k+m)

-- Requires defining type-level/compile-time addition!
-- Which a priori isn't commutative!

class (Graded f) Operad (f :: Nat -> *) where
  ident :: f (S Z)
  compose :: f n -> Forest f m n -> f m

-- Very reasonable, but one must notice that the total arity m is
-- not yet partitioned as m = n1 + ... + nk, as in the traditional operad
-- definition.

class Graded f where
  grade :: f a -> Nat

data SNat n where
  SZ :: SNat Z -- "force" inhabittance?
  SS :: SNat n -> S (SNat n)

type family (+) :: (a :: Nat) -> (b :: Nat) -> (c :: Nat)
  Z + n = n
  (S n) + m = S (n + m)

-- The _cheating_ in order to make type-level addition commute:

plusZ :: forall n. Dict (n ~ (n + Z))
plusZ = unsafecoerce (Dict (n ~ (n + Z)))

-- The _helper class_ Forest for operads:
-- Pure recall, this might be very wrong...

class (Operad f) Forest f m n where
  Nil :: Forest f Z Z
  compose :: f i1 -> Forest f i2 n -> Forest f (i1 + i2) (S n)

instance Forest (MoveTree m, n :: Nat) where
  Nil = (Fan NilT, Z)
  compose -- ... Failed...

-- fff

instance Operad MoveTree where
  ident = Leaf
  compose Leaf t = t
  compose
