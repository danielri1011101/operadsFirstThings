module Exercise5 (permuteFile) where

lcg :: Int -> [Int]
lcg seed =
  let next = (1103515245 * seed + 12345) `mod` 2147483648
  in next : lcg next

shuffle :: Int -> [a] -> [a]
shuffle _ [] = []
shuffle seed xs =
  let i        = seed `mod` length xs
      (antes, y:después) = splitAt i xs
      nextSeed = head (lcg seed)
  in y : shuffle nextSeed (antes ++ después)


permuteFile :: IO ()
permuteFile = do
  putStrLn "Ruta del archivo a barajar:"
  filePath  <- getLine
  contenido <- readFile filePath
  let líneas = lines contenido
  length contenido `seq`
    writeFile filePath (unlines (shuffle 42 líneas))

  putStrLn "¡Archivo barajado!"
