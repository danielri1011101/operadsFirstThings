module Main where

import PermFile
import System.Random

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  _ <- someDigits
  r_g <- getStdGen
  let nrs = permuteNrs r_g 10
  putStrLn $ show nrs
  let c_ity = permuteList "florencia" r_g
  putStrLn c_ity
  putStrLn "Bye, Haskell!"
