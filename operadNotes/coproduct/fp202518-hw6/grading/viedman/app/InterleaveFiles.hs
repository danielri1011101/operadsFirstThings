module InterleaveFiles (inter_leave) where

import Control.Exception (IOException, try)
import System.Environment (getArgs)
import System.IO

inter_leave :: IO ()
inter_leave = do
  filePaths <- getArgs
  handles <- mapM (`openFile` ReadMode) filePaths
  interleave handles
  mapM_ hClose handles

interleave :: [Handle] -> IO ()
interleave handles = do
  results <- mapM safeHGetLine handles
  let validLines = [line | Just line <- results]
  mapM_ putStrLn validLines
  let remainingHandles = [h | (h, Just _) <- zip handles results]
  if null remainingHandles
    then return ()
    else interleave remainingHandles

safeHGetLine :: Handle -> IO (Maybe String)
safeHGetLine h = do
  result <- try (hGetLine h) :: IO (Either IOException String)
  case result of
    Left _ -> return Nothing
    Right line -> return (Just line)
