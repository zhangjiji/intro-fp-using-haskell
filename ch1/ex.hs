
quad :: Int -> Int
quad x = square (square x)
  where square y = y * y

greater :: (Int, Int) -> Int
greater (x,y) = if x > y then x else y

areaOfCircle :: Double -> Double
areaOfCircle r = r * r * 22 / 7
