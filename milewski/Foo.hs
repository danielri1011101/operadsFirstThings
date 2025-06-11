import Numbers

head' :: [a] -> a
head' xs =
  case xs of [] -> error "Empty list!"
             y:ys -> y



-- Should I bind n?
-- We call splitForest for _some_ splitting of the inputs...
splitForest :: forall f m n r i. SNat m -> SNat n -> Forest f i (m+n) ->
  (
    forall i' i''. (i' + i'') ~ i =>
    (Forest f i' m, Forest f i'' n) -> r
  ) -> r
splitForest (SS (sm :: SNat m)) sn
            (Cons (t :: f i1) (ts :: Forest f i2 (m+n))) k =
  splitForest sm sn $
    (-- arg is a splitting of ts.
      \ (uts :: Forest f i3 m, vts :: Forest f i4 n) ->
          (Cons t uts, vts)
    ) -- essentially, I guess.
