class Coproduct u where
  incll :: a -> u a b
  inclr :: b -> u a b
  cmap :: (a -> z) -> (b -> z) -> (u a b -> z)

instance Coproduct Either where
  incll = Left
  inclr = Right
  cmap f g e =
    case e of
      Left x -> f x
      Right y -> g y

-- Binary relations
newtype Rel a = R {rel :: a -> a -> Bool}

-- Equivalence relations
class Erel r a where
  ref :: r -> a -> Bool
  sim :: r -> a -> a -> Bool
  trn :: r -> a -> a -> a -> Bool

instance Erel (Rel a) a where
  ref f x = x `r` x
    where r = rel f
  sim f x y = if (x `r` y) then (y `r` x) else True
    where r = rel f
  trn f x y z = if (x `r` y) && (y `r` z) then (x `r` z) else True
    where r = rel f

-- Rose trees
data Rose a = Nil | Rs (RTree a)
data RTree a = Node a [RTree a]

--
-- 25.08.19 White operads.
-- Everyting is "dirty" because Int should be Nat, and
-- Int -> Int should be permutations of [1..n]
--

-- Pre-operad: a collection of "sets"; s a :: set of a's
newtype PO s a = PO {coll :: Int -> s a}

-- Free operad
class FreeOperad f s a where
---- corollas
  cor :: Int -> PO s a -> f s a
---- grafting
  grf :: Int -> f s a -> f s a -> f s a
---- composition unit
  one :: a
---- S_n^{op} action.
  act :: (Int -> Int) -> f s a -> f s a

-- Tree operads, the working instance of free operads
newtype TrOp a = T {tree :: (Rose a, Int)}

-- Operads
-- Definied as quotients of their free operads
class Operad s a where
  eps :: TrOp a -> PO s a
