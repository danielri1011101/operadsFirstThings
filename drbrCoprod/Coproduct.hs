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
