import TupleFunctions

data Liste a = Nil | Snoc (Liste a) a
            deriving (Show)

head' :: Liste a -> a
head' (Snoc Nil x) = x
head' (Snoc xs x) = head' xs

-- 4.3.9

pairs n = [(x,y) | x <- [1..n], y <- [x..n]]

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = []: map (x:) (inits xs)

inits' = foldr f [[]] where f x xss = []: map (x:) xss

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : (tails xs)

tails' = foldl (flip f) [[]]
         where f x xss = [] : map (x:) xss

filter' p = foldr f []
  where f x xs = if p x then x : xs else xs

dot x y = max 0 (x+y)

maxlist :: Ord a => [a] -> a
maxlist = foldr1 max

segs = concat . map inits . tails

mss = maxlist . scanr dot 0

main :: IO ()
main = return ()
