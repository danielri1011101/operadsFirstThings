module InterleaveFiles (inter_leave) where

--runhaskell exercise10_06.hs colores.txt animales.txt numeros.txt

import System.Environment
import System.IO

inter_leave = do
    archivos <- getArgs
    if null archivos
        then putStrLn "necesito al menos un archivo"
        else do
            todasLineas <- mapM leerTodo archivos
            intercalar todasLineas

leerTodo :: String -> IO [String]
leerTodo archivo = do
    contenido <- readFile archivo
    return (lines contenido)

tomarPrimeros :: [[String]] -> [String]
tomarPrimeros listas = [head lista | lista <- listas, not (null lista)]

quitarPrimeros :: [[String]] -> [[String]]
quitarPrimeros listas = [if null lista then [] else tail lista | lista <- listas]
 
hayMas :: [[String]] -> Bool
hayMas listas = any (not . null) listas

-- intercalar 
intercalar :: [[String]] -> IO ()
intercalar listas = do
    let primeros = tomarPrimeros listas
    if null primeros
        then return ()  -- terminamos
        else do
            -- imprimir las primeras lineas
            mapM_ putStrLn primeros
            -- continuar con el resto
            intercalar (quitarPrimeros listas)
