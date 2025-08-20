module PermuteFile (permuteFile) where

import System.Environment (getArgs)
import System.Random      (getStdRandom, randomR)
import System.IO          (readFile, writeFile)

shuffle :: [a] -> IO [a]
shuffle []  = return []
shuffle [x] = return [x]
shuffle xs  = do
 
    i <- getStdRandom (randomR (0, length xs - 1))
    let (left, x : right) = splitAt i xs
    rest <- shuffle (left ++ right)
    return (x : rest)


permuteFile :: IO ()
permuteFile = do
    args <- getArgs
    case args of
        [fp] -> do
            contents <- readFile fp
            shuffled <- shuffle (lines contents)
            writeFile fp (unlines shuffled)
        _    -> putStrLn "Uso: shuffle <ruta-del-archivo>"
