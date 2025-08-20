module Assignment6 (permuteFile, inter_leave) where

import System.Random
import System.IO
import Control.Monad
import Control.Exception (evaluate)


-- 10.05
permuteFile :: IO ()
permuteFile = do
    filePath <- getLine
    totalLines <- readFileLines filePath

    let n = length totalLines

    g <- getStdGen
    newStdGen

    let shuffleContent = shuffle g 1 n totalLines

    writeFile filePath (unlines shuffleContent)

-- 10.06
inter_leave :: IO ()
inter_leave = do
    filePaths <- getLine
    let files = words filePaths
    filesContent <- mapM readFileLines files
    let n = length filesContent

    putStrLn $ unlines $ printInterludeLines 0 n filesContent []

readFileLines :: String->IO [String]
readFileLines file = do
    handle <- openFile file ReadMode
    fileContent <- hGetContents handle
    evaluate (length fileContent) 
    hClose handle

    return (lines fileContent)

printInterludeLines :: Int->Int->[[String]]->[String]->[String]
printInterludeLines i n [] accum = accum
printInterludeLines i n files accum = 
    let hs = take i files
        ts = drop i files
        file = head ts
        filteredFiles = filter (not.null) (hs ++ [(tail file)] ++ (tail ts))
        m = length filteredFiles
    in printInterludeLines (mod (i+1) m) m filteredFiles (accum ++ [(head file)]) 

shuffle :: StdGen ->Int->Int->[a]->[a]
shuffle g i n accum 
    | i > n = accum
    | otherwise = shuffle g (i+1) n (swap r i accum) where r = fst $ randomR (1, i) g

swap :: Int->Int->[a]->[a]
swap id1 id2 xs 
    | id1 == id2 = xs
    | otherwise = 
        let (h:hs) = drop (id1-1) xs
            ms = take (id2 - id1 - 1) hs
            (t:ts) = drop (id2-1) xs
        in (take (id1-1) xs) ++ [t] ++ ms ++ [h] ++ ts

    
