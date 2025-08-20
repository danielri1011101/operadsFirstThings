module Main where

import PermuteFile
import InterleaveFiles

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  --permuteFile
  inter_leave
  putStrLn "Bye, Haskell!"
