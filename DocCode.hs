data MoveTree n where
  Leaf :: MoveTree One
  Fan :: Trees n -> MoveTree n

data Trees n where
  NilT :: Trees Z
  (:+) :: (Move, MoveTree a) -> Trees b -> Trees (a+b)

class (Graded f) Operad where
  ident :: f One
  compose :: f n -> Forest f m n -> fm

data Forest f m n where
  Nil :: Forest f Z Z
  Cons :: f i1 -> Forest f i2 n -> Forest f (i1+i2) (S n)

splitForest :: forall f a b z q. SNat a -> SNat b ->
  Forest f q (a+b) ->
  (
   forall j j'. (j+j') ~ q =>
   (Forest f j a, Forest f j' b) -> z
  ) -> z
splitForest (SS (sl :: SNat l))
            (sk :: SNat k)
            (Cons (t :: f j1) (frt :: Forest f j2 (l+k)))
            c =
  splitForest sl sk frt $
    (
     \((lrdr :: Forest f j2' l),(krdr :: Forest f j2'' k)) ->
        case plusAssoc (j1 :: Proxy j1)
                       (j2' :: Proxy j2')
                       (j2'' :: Proxy j2'') of
        Dict -> c (Cons t lrdr , krdr)
    )

instance Operad MoveTree where
  ident = Leaf
  compose Leaf (Cons (t :: MoveTree m) Nil) =
    case plusZ :: Dict (m ~ (m + Z)) of Dict -> t
  compose (Fan ((mv, t) :+ ts)) frt =
    let ans = splitForest (grade t) (grade ts) frt lambda
        lambda = \(mst1, mst2) -> Fan ((mv,tree) :+ trees)
        tree = compose t mst1
        (Fan trees) = compose (Fan ts) mst2
    in ans
  compose _ _ = error "Composition undefined!"

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)

class Functor m => Monad m where
  return :: a -> m a
  join :: m (m a) -> m a

data M f a where
  M :: f n -> Vec n a -> M f a

newtype W f a = W {runW :: forall n. f n -> Vec n a}

-- drbr predicts:
runW :: W -> forall n. f n -> Vec n a
--    Polymorphic, I guess.
--    Not what comes out in ghci, sorry.

-- ghci says:
runW :: W f a -> forall (n :: Nat). f n -> Vec n a
-- meaning _n of Nat kind_, I.g.

-- drbr predicts:
W :: (forall (n :: Nat). f n -> Vec n a) -> W f a
-- i.e. W is a placeholder for a tree-to-vector device...
-- Coincides with ghci's answer.

-- Logging dialog with ghci:

:k W
> (Nat -> *) -> * -> *

instance Functor (Vec n) where
  fmap f VNil = VNil
  fmap f (VCons a vs) = VCons (f a) (fmap f vs)

instance Functor (W f) where
  fmap g (W k) =
    W (
       \ f_n -> fmap g (k f_n)
      )

extract :: W f a -> a
extract (W k) = case k ident of VCons a0 VNil -> a0

duplicate' :: W f a -> W f (W f a)
