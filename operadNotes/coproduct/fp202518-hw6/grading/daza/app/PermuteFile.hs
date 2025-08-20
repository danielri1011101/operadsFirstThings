module PermuteFile (permuteFile) where

--runhaskell exercise10_05.hs archivo1.txt
import System.Environment
import System.Random
import Control.Monad

permuteFile = do
    [archivo] <- getArgs
    generador <- getStdGen
    contenido <- readFile archivo
    let lineasArchivo = lines contenido
    mezclado <- mezclarLineas lineasArchivo generador
    writeFile archivo (unlines mezclado)

mezclarLineas :: [String] -> StdGen -> IO [String]
mezclarLineas [] _ = return []
mezclarLineas [x] _ = return [x]  -- caso base: solo una linea
mezclarLineas xs gen = do
    let (indice, nuevoGen) = randomR (0, length xs - 1) gen
    let (inicio, y:final) = splitAt indice xs
    resto <- mezclarLineas (inicio ++ final) nuevoGen
    return (y:resto)
