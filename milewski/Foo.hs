import Numbers

head' :: [a] -> a
head' xs =
  case xs of [] -> error "Empty list!"
             y:ys -> y
