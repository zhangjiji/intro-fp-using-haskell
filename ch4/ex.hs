data Liste a = Nil | Snoc (Liste a) a
            deriving (Show)

head' :: Liste a -> a
head' (Snoc Nil x) = x
head' (Snoc xs x) = head' xs

-- 4.3.9

pairs n = [(x,y) | x <- [1..n], y <- [x..n]]
