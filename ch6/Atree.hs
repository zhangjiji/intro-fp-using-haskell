
data Atree a = Leaf a | Fork Int (Atree a) (Atree a)
             deriving(Show)
fork :: Atree a -> Atree a -> Atree a
fork xt yt = Fork n xt yt
  where n = lsize xt

lsize :: Atree a -> Int
lsize (Leaf a) = 1
lsize (Fork n xt yt) = lsize xt + lsize yt

mkAtree :: [a] -> Atree a
mkAtree xs
  | (m == 0) = Leaf (unwrap xs)
  | otherwise = fork (mkAtree ys) (mkAtree zs)
  where m = (length xs) `div` 2
        (ys, zs) = splitAt m xs

wrap x = [x]
unwrap [x] = x

retrieve :: Atree a -> Int -> a
retrieve (Leaf x) 0 = x
retrieve (Fork m xt yt) k = if k < m then retrieve xt k else retrieve yt (k - m)

