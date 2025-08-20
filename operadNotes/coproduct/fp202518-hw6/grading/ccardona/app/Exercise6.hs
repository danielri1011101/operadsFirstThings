module Exercise6 (inter_leave) where

interleave :: [[String]] -> [String]
interleave xss
  | all null xss = []
  | otherwise    =
      [ head xs | xs <- xss, not (null xs) ]
      ++ interleave [ tail xs | xs <- xss, not (null xs) ]

inter_leave :: IO ()
inter_leave = do
  putStrLn "Rutas de archivos (separadas por espacios):"
  line <- getLine
  let paths = words line
  contenidos <- mapM readFile paths       -- Prelude tiene readFile
  let listasDeLíneas = map lines contenidos
      resultado      = interleave listasDeLíneas
  putStrLn $ unlines resultado
