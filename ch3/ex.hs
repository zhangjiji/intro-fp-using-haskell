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

newtype Rational = Rat (Integer, Integer)

mkRat :: (Integer, Integer) -> Main.Rational
mkRat (x,y) = Rat (u `div` d, v `div` d)
  where u = (signum y) * x
        v = abs y
        d = gcd x y

instance Eq Main.Rational where
  Rat (x,y) == Rat (u,v) = x*v == y*u

instance Ord Main.Rational where
  Rat (x,y) < Rat (u,v) = x*v < y*u

showRat :: Main.Rational -> String
showRat (Rat (x,y)) = if y == 1 then show x
                    else show x ++ "/" ++ show y
