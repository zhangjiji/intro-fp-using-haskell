
data Stree a = Null | Fork (Stree a) a (Stree a)
             deriving (Show)

wrap x = [x]

foldStree e f g Null = e
foldStree e f g (Fork xt a yt) = g (g (foldStree e f g xt) (f a)) (foldStree e f g yt)

flatten :: Stree a -> [a]
flatten Null = []
flatten (Fork xt a yt) = flatten xt ++ [a] ++ flatten yt

flatten' = foldStree [] wrap (++)

member :: (Eq a) => a -> Stree a -> Bool
member x = foldStree False check (||)
  where check y = (x == y)

member' :: (Ord a) => a -> Stree a -> Bool
member' x Null = False
member' x (Fork xt a yt)
  | x < a = member' x xt
  | x == a = True
  | x > a = member' x yt

height :: Stree a -> Integer
height Null = 0
height (Fork xt a yt) = 1 + (max (height xt) (height yt))

mkStree :: (Ord a) => [a] -> Stree a
mkStree [] = Null
mkStree (x:xs) = Fork (mkStree ys) x (mkStree zs)
  where (ys, zs) = partition (<= x) xs

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' p = foldr f ([],[])
  where f x res = if p x then cross ((x:),id) res
                  else cross (id, (x:)) res

pair (f,g) x = (f x, g x)
cross (f,g) (x,y) = (f x, g y)

sort' :: (Ord a) => [a] -> [a]
sort' = flatten . mkStree

insert :: (Ord a) => a -> Stree a -> Stree a
insert x Null = Fork Null x Null
insert x (Fork xt a yt)
  | x < a = Fork (insert x xt) a yt
  | x == a = Fork xt a yt
  | x > a = Fork xt a (insert x yt)

delete :: (Ord a) => a -> Stree a -> Stree a
delete x Null = Null
delete x (Fork xt a yt)
  | (x < a) = Fork (delete x xt) a yt
  | (x == a) = join' xt yt
  | (x > a) = Fork xt a (delete x yt)

headTree :: Stree a -> a
headTree = head . flatten

tailTree :: (Ord a) => Stree a -> Stree a
tailTree = mkStree . tail . flatten

join' xt yt = if empty yt then xt
              else Fork xt (headTree yt) (tailTree yt)
  where empty :: Stree a -> Bool
        empty Null = True
        empty (Fork xt a yt) = False

splitTree :: (Ord a) => Stree a -> (a, Stree a)
splitTree = pair (headTree, tailTree)

mapTree :: (a -> b) -> Stree a -> Stree b
mapTree f Null = Null
mapTree f (Fork xt a yt) = Fork (mapTree f xt) (f a) (mapTree f yt)
