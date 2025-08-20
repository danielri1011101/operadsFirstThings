permuteList :: [a] -> [a]
permuteList xs = p_map (pmt n) xs
  where
    p_map :: [Int] -> [a] -> [a] 
    p_map ns xs = foldr ((:) . (xs!!)) [] ns
    (pmt, n) = (permuteNrs, length xs)

permuteNrs :: Int -> [Int]
permuteNrs = \ _ -> [1..10]
