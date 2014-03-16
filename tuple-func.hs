pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f,g) x = (f x, g x)

cross :: (a -> b, c -> d) -> (a,c) -> (b,d)
cross (f,g) = pair (f.fst, g.snd)

case' :: (a -> c, b -> c) -> Either a b -> c
case' (f,g) (Left x) = f x
case' (f,g) (Right y) = g y

plus :: (a -> b, c -> d) -> Either a c -> Either b d
plus (f,g) = case' (Left . f, Right . g)
