import Data.Char(chr,ord)

nextlet :: Char -> Char
nextlet c
  | (ordc >= ord 'A' && ordc < ord 'Z') || (ordc >= ord 'a' && ordc < ord 'z') = chr (ordc + 1)
  | (ordc == ord 'Z') || (ordc == ord 'z') = chr (ordc + 1 - 26)
  | otherwise = error "not character"
  where ordc = ord c

digitval :: Char -> Int
digitval d = (ord d) - (ord '0')

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
         deriving (Eq, Ord, Enum, Show)

-- 2.3.1
dayBefore :: Day -> Day
dayBefore d = toEnum (((fromEnum d) + 6 ) `mod` 7)


data Direction = North | East | South | West
               deriving (Eq, Ord, Enum, Show)

-- 2.3.2
reverseDir :: Direction -> Direction
reverseDir d = toEnum (((fromEnum d) + 2) `mod` 4)

{-
instance Enum Bool where
  toEnum False = 0
  toEnum True = 1
  fromEnum 0 = False
  fromEnum 1 = True
-}

-- 2.4.2
data Triple a b c = MkTriple a b c
                  deriving (Show)

birthYears :: (Int, Int, Int) -> (Int, Int, Int) -> Int
birthYears (dNow,mNow,yNow) (dBirth, mBirth, yBirth) =
  (yNow - yBirth)

