-- extract is the counit.
-- duplicate is the comultiplication.
class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)

-- An "on the fly" monad for _dually replicating_ the
-- Comonad definitions.
class Functor m => Moonad m where
  yoin :: m (m a) -> m a
  riturn :: a -> m a
