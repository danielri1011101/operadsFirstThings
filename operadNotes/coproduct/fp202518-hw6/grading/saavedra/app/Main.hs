module Main where

import InterleaveFiles
import PermuteFile

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
--  permuteFile
  inter_leave
  putStrLn "Bye, Haskell!"
