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
