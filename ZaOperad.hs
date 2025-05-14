module Operad where

data Op a = () | Op a

type OpIndexing a = Int -> Op a

class PreOperad a where
  f :: Int -> Op a
  |  n < 0 = ()

-- Impose PreOperad constraint on OpIndexing.
