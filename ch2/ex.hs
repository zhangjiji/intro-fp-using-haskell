import Data.Char(chr,ord)

nextlet :: Char -> Char
nextlet c
  | ordc >= ord 'z' = chr (ordc + 1 - 26)
  | ordc >= ord 'a' = chr (ordc + 1)
  | ordc >= ord 'Z' = chr (ordc + 1 - 26)
  | ordc >= ord 'A' = chr (ordc + 1)
  | otherwise = error "not character"
  where ordc = ord c
