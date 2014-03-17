
units,teens,tens :: [String]
units = ["zero","one","two","three","four","five","six","seven","eight","nine"]
teens = ["ten","eleven","twelve","thirteen","fourten","fifteen","sixteen","seventeen","eighteen","nineteen"]
tens = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

convert2 :: Int -> String
convert2 = combine2 . digits2

digits2 :: Int -> (Int, Int)
digits2 n = (n `div` 10, n `mod` 10)

combine2 :: (Int,Int) -> String
combine2 (0, u) = units !! u
combine2 (1, u) = teens !! u
combine2 (t, 0) = tens !! (t-2)
combine2 (t, u) = tens !! (t-2) ++ "-" ++ units !! u

convert3 :: Int -> String
convert3 = combine3 . digits3

digits3 :: Int -> (Int, Int)
digits3 n = (n `div` 100, n `mod` 100)

combine3 :: (Int,Int) -> String
combine3 (0, t) = convert2 t
combine3 (h, 0) = units !! h ++ " hundred"
combine3 (h, t) = units !! h ++ " hundred and " ++ convert2 t

convert6 :: Int -> String
convert6 = combine6 . digits6

digits6 :: Int -> (Int, Int)
digits6  n = (n `div` 1000, n `mod` 1000)

combine6 :: (Int, Int) -> String
combine6 (0, h) = convert3 h
combine6 (t, 0) = convert3 t ++ " thousand"
combine6 (t, h) = convert3 t ++ " thousand" ++ link h ++ convert3 h

link :: Int -> String
link h = if h < 100 then " and " else " "

convert12 :: Int -> String
convert12 n
  | n > 1000 = convert12 (n `div` 1000)++ " thousand " ++ convert6 (n `mod` 1000)
  | otherwise = convert6 n
