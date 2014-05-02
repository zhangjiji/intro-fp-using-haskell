data Htree a = Null | Fork a (Htree a) (Htree a)

flatten :: (Ord a) => Htree a -> [a]
flatten Null = []
flatten (Fork a xt yt) = a : merge (flatten xt) (flatten yt)

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge (x:xs) [] = x:xs
merge (x:xs) (y:ys) = if (x <= y) then x: (merge xs (y:ys))
                      else y: (merge (x:xs) ys)

mkHeap :: (Ord a) => [a] -> Htree a
mkHeap = heapify . mkHtree

heapify :: Htree a -> Htree a
heapify Null = Null
heapify (Fork x xt yt) = sift x (heapify xt) (heapify yt)

sift :: a -> Htree a -> Htree a -> Htree a
sift x Null Null = Fork x Null Null
sift x (Fork y a b) Null = if x <= y then Fork x (Fork y a b) Null
                           else Fork y (sift x a b) Null
sift x Null (Fork z c d) = if x <= z then Fork x Null (Fork z c d)
                           else Fork z Null (sift x c d)
sift x (Fork y a b) (Fork z c d)
  | x <= (min y z) = Fork x (Fork y a b) (Fork z c d)
  | y <= (min x z) = Fork y (sift x a b) (Fork z c d)
  | z <= (min x y) = Fork z (Fork y a b) (sift x c d)
