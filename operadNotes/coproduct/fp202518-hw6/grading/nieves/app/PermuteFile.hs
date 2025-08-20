module PermuteFile (permuteFile) where

import System.Environment (getArgs)

fakeShuffle :: [a] -> [a]
fakeShuffle xs = rotate (length xs `mod` 3) xs
  where
    rotate 0 ys = ys
    rotate n ys = rotate (n - 1) (tail ys ++ [head ys])

permuteFile :: IO ()
permuteFile = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- readFile filePath
      let ls = lines content
          shuffled = fakeShuffle ls
      writeFile filePath (unlines shuffled)
    _ -> putStrLn "Usage: runghc 05.hs <filename>"
