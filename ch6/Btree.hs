module Btree where

data Btree a = Leaf a | Fork (Btree a) (Btree a)
             deriving (Show)
                      
-- size :: Btree a -> Int
-- size (Leaf x) = 1
-- size (Fork xt yt) = size xt + size yt

-- mapBtree :: (a -> b) -> Btree a -> Btree b
-- mapBtree f (Leaf x) = Leaf (f x)
-- mapBtree f (Fork xt yt) = Fork (mapBtree f xt) (mapBtree f yt)

foldBtree :: (a -> b) -> (b -> b -> b) -> (Btree a) -> b
foldBtree f g (Leaf x) = f x
foldBtree f g (Fork xt yt) = g (foldBtree f g xt) (foldBtree f g yt)

mapBtree f = foldBtree (Leaf .f) Fork

size = foldBtree (const 1) (+)
height = foldBtree (const 0) addMax
  where addMax x y = 1 + (max x y)

flatten = foldBtree wrap (++)

wrap x = [x]
unwrap [x] = x

maxBtree :: (Ord a) => Btree a -> a
maxBtree = foldBtree id max

mkBtree :: [a] -> Btree a
mkBtree xs
  | (m == 0) = Leaf (unwrap xs)
  | otherwise = Fork (mkBtree ys) (mkBtree zs)
  where m = (length xs) `div` 2
        (ys, zs) = splitAt m xs
