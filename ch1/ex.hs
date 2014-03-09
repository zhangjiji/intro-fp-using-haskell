
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

integral :: a -> b -> (r -> r)
integral a b f = undefined

fib :: Int -> Int
fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = (fib (n - 1)) + (fib (n - 2))

abs :: Int -> Int
abs n
  | n < 0 = (-n)
  | n == 0 = 0
  | n > 0 = n
