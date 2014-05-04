module Queue (Queue, empty, isEmpty, joinQ, front, back) where

newtype Queue a = MkQ ([a],[a])

empty :: Queue a
empty = MkQ ([], [])

isEmpty :: Queue a -> Bool
isEmpty (MkQ (xs, ys)) = null xs

joinQ :: a -> Queue a -> Queue a
joinQ x (MkQ (xs, ys)) = mkValid (x:xs, ys)

front :: Queue a -> a
front (MkQ (x:xs, ys)) = x

back :: Queue a -> Queue a
back (MkQ (x:xs, ys)) = mkValid (xs,ys)

mkValid :: ([a], [a]) -> Queue a
mkValid (xs, ys) = if null xs then MkQ (reverse ys, [])
                   else MkQ (xs, ys)
