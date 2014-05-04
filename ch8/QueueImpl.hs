import Queue

toQ :: [a] -> Queue a
toQ =  foldr joinQ empty . reverse

fromQ :: Queue a -> [a]
fromQ q = if isEmpty q then []
          else front q : fromQ (back q)
