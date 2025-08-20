module PermuteFile (permuteFile) where

import Control.Exception (evaluate)
import System.Environment (getArgs)

permuteFile :: IO ()
permuteFile = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- readFile filePath
      evaluate (length content)
      let ls = lines content
          shuffled = everyOther ls ++ remaining ls
            where
              everyOther xs = [x | (x, i) <- zip xs [0 ..], even i]
              remaining xs = [x | (x, i) <- zip xs [0 ..], odd i]
      writeFile filePath (unlines shuffled)
