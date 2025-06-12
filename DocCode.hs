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
splitForest (SS (sl :: SNat l)) sk frt
