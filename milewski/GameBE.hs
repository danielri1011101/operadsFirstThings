{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

import LinAlg
import Numbers
import Trees

-- This is the Game Back-End module.

instance Show Player where
  show Cross = " X "
  show Circle = " O "

toFin3 :: Int -> Maybe (Fin Three)
toFin3 0 = Just FinZ
toFin3 1 = Just (FinS FinZ)
toFin3 2 = Just (FinS (FinS FinZ))
toFin3 _ = Nothing

type Board = Matrix Three Three (Maybe Player)
