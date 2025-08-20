module InterleaveFiles (inter_leave) where

import System.Environment (getArgs)
import System.IO

inter_leave :: IO ()
inter_leave = do
  filePaths <- getArgs
  handles <- mapM (`openFile` ReadMode) filePaths
  interleave handles
  mapM_ hClose handles

-- Imprime una línea por archivo, intercaladamente
interleave :: [Handle] -> IO ()
interleave hs = do
  lines <- mapM safeGetLine hs
  let nonEmpty = [l | Just l <- lines]
  if null nonEmpty
    then return ()
    else do
      mapM_ (maybe (return ()) putStrLn) lines
      interleave hs

-- Lee una línea si no es EOF, si no devuelve Nothing
safeGetLine :: Handle -> IO (Maybe String)
safeGetLine h = do
  eof <- hIsEOF h
  if eof then return Nothing else Just <$> hGetLine h
