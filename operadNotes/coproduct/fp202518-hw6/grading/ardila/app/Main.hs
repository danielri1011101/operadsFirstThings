module Main where

import ShuffleText
import System.Random
import System.Random (StdGen (..))

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  shuffle_text
  putStrLn "Bye, Haskell!"
