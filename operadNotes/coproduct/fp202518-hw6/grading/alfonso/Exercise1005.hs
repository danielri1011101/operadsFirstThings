import System.Environment (getArgs) 
import System.IO (readFile, writeFile) 
import System.Random (randomRIO) 
import Data.List (sortBy) 
import Control.Monad (forM) 
import Data.Ord (comparing) 

shuffleList :: [a] -> IO [a]
shuffleList xs = do
    pairs <- forM xs $ \x -> do
        r <- randomRIO (0.0 :: Double, 1.0 :: Double)
        return (r, x)

    let shuffledPairs = sortBy (comparing fst) pairs

    return $ map snd shuffledPairs

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            content <- readFile filePath
            let linesList = lines content
            shuffledLines <- shuffleList linesList
            let newContent = unlines shuffledLines
            writeFile filePath newContent
            putStrLn $ "Líneas desordenadas exitosamente en: " ++ filePath
        _ -> do
            putStrLn "Uso: runghc ShuffleFile.hs <ruta_del_archivo>"
            putStrLn "Ejemplo: runghc ShuffleFile.hs mi_documento.txt"

