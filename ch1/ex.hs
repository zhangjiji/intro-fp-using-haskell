
quad :: Int -> Int
quad x = square (square x)
  where square y = y * y

greater :: (Int, Int) -> Int
greater (x,y) = if x > y then x else y

areaOfCircle :: Double -> Double
areaOfCircle r = r * r * 22 / 7

uncurry :: (a -> b -> c) -> ((a,b)->c)
uncurry f (x,y) = f x y

curry :: ((a,b)->c) -> (a->b->c)
curry f x y = f (x,y)

