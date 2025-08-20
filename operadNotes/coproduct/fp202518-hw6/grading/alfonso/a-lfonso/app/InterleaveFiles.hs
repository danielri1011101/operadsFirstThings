module InterleaveFiles (inter_leave) where

-- Importa modulos necesarios
import System.Environment (getArgs)  
import System.IO   

interleaveHandles :: [Handle] -> IO ()
interleaveHandles [] = return ()
interleaveHandles handles = do
    results <- mapM readLineFromHandle handles
    let linesToPrint = [line | Just line <- results]
    if null linesToPrint
        then do
            mapM_ hClose handles
            return ()
        else do
            mapM_ putStrLn linesToPrint
            let remainingHandles = [h | (Just _, h) <- zip results handles]
            interleaveHandles remainingHandles

readLineFromHandle :: Handle -> IO (Maybe String)
readLineFromHandle h = do
    isEOF <- hIsEOF h 
    if isEOF
        then return Nothing 
        else do
            line <- hGetLine h 
            return (Just line) 

inter_leave :: IO ()
inter_leave = do
    filePaths <- getArgs
    if null filePaths
        then do

            putStrLn "Uso: runghc InterleaveFiles.hs <ruta_archivo1> [ruta_archivo2 ...]"
            putStrLn "Ejemplo: runghc InterleaveFiles.hs file1.txt file2.txt"
        else do
            handles <- mapM (`openFile` ReadMode) filePaths
            interleaveHandles handles
