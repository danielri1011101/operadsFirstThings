module ShuffleText (shuffle_text) where

import System.IO
import System.Random
--import System.Random (randomR, getStdGen)

shuffle gen [] = ([], gen)
shuffle gen xs =
  let (idx, gen') = randomR (0, length xs - 1) gen
      (left, (a:right)) = splitAt idx xs
      (restShuffled, gen'') = shuffle gen' (left ++ right)
  in (a : restShuffled, gen'')

randomizeFile :: StdGen -> IO [String]
randomizeFile gen = do
    content <- readFile "file.txt"
    let (shuffled, _) = shuffle gen (lines content)
    return shuffled

shuffle_text :: IO ()
shuffle_text = do
    gen <- getStdGen
    linesOfFile <- randomizeFile gen
    writeFile "shuffled_file.txt" (unlines linesOfFile)
