import Btree

data Rose a = Node a [Rose a]

foldRose :: (a -> [b] -> b) -> Rose a -> b
foldRose f (Node x xts) = f x (map (foldRose f) xts)

mapRose :: (a -> b) -> Rose a -> Rose b
mapRose f = foldRose (Node . f)

size = foldRose f where f x ns = 1 + sum ns

toB :: Rose a -> Btree a
toB (Node x xts) = foldl Fork (Leaf x) (map toB xts)

toB' (Node x xts) = foldr (flip Fork) (Leaf x) (reverse (map toB xts))

toR (Leaf x) = Node x []
toR (Fork xb yb) = Node x (xts ++ [xt])
  where Node x xts = toR xb
        xt = toR yb
