class (Functor w) => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
