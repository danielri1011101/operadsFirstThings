module Operad where

data Tau a = () | T a -- Tau for _truncation_...

nNats :: Int -> Tau Int
|  n < 0 = ()
| otherwise = T n

type OpIx a = Tau Int -> Tau a

-- Then, a pre-operad could be the precomposition of an opix with nNats...

functor (getPreOp a) :: Int -> OpIx a -> Tau a where -- kind of a functor...
\n f -> f (nNats n)

-- dude, but I _do_ have the type of _lists_ of type A, and those are "whatever length..."
