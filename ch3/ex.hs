
data Nat = Zero | Succ Nat
         deriving (Eq, Ord, Show)

--3.1.5
minus :: Nat -> Nat -> Nat
minus (Succ n) Zero = Succ n
minus (Succ n) (Succ m)
  | n < m = Zero
  | otherwise = minus n m

--3.1.1 Positive Numbers
--data PNum = Nat | 
  
--3.1.2
convert :: Nat -> Integer
convert Zero = 0
convert (Succ n) = 1 + convert n

main :: IO ()
main = print "main"
