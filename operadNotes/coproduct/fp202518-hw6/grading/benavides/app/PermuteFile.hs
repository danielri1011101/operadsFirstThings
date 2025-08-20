module PermuteFile (permuteFile) where

import System.Environment (getArgs)
import System.Random (randomRIO)
import Control.Monad (forM)
import Data.List (sortOn)

shuffle :: [a] -> IO [a]
shuffle xs = do
  rands <- mapM (\_ -> randomRIO (1, 1000000 :: Int)) xs
  return (map snd (sortOn fst (zip rands xs)))

permuteFile :: IO ()
permuteFile = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      let ls = lines contents
      shuffled <- shuffle ls
      writeFile filePath (unlines shuffled)
    _ -> putStrLn "Uso: shufflelines <file>"
