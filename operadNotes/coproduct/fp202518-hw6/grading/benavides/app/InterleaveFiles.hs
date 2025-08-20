module InterleaveFiles (inter_leave) where

import System.Environment (getArgs)
import System.IO
import Control.Monad (forM, when)
import Data.List (transpose)

readLines :: Handle -> IO [String]
readLines h = do
  eof <- hIsEOF h
  if eof
    then return []
    else do
      line <- hGetLine h
      rest <- readLines h
      return (line : rest)

interleave :: [[a]] -> [a]
interleave [] = []
interleave xs = concat (transpose xs)

inter_leave :: IO ()
inter_leave = do
  files <- getArgs
  handles <- mapM (`openFile` ReadMode) files
  contents <- mapM readLines handles
  let result = interleave contents
  mapM_ putStrLn result
  mapM_ hClose handles
