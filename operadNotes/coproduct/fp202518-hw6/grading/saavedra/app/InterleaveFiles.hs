module InterleaveFiles (inter_leave) where

import System.Environment (getArgs)
import System.IO
       ( IOMode(ReadMode), hIsEOF, hGetLine, hClose
       , openFile, stdout, hPutStrLn, Handle )
import Control.Monad      (filterM, forM_)


roundRobin :: [Handle] -> IO ()
roundRobin []  = return ()                        
roundRobin hs  = do

  alive <- filterM (fmap not . hIsEOF) hs
  if null alive
     then mapM_ hClose hs                          
     else do
   
       forM_ alive $ \h -> do
         line <- hGetLine h
         hPutStrLn stdout line

       roundRobin hs

inter_leave :: IO ()
inter_leave = do
  fps <- getArgs
  if null fps
     then putStrLn "Uso: interleave <archivo1> <archivo2> ..."
     else do
        handles <- mapM (`openFile` ReadMode) fps
        roundRobin handles
