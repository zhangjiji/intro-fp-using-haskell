
import TupleFunctions

type Name = String
type Iden = Integer
type Mark = Int
type Rank = Int

type Codes = [(Name, Iden)]
type Marks = [(Iden, Mark)]
type Ranks = [(Name, Mark, Rank)]

classlist :: (Codes, Marks) -> Ranks
classlist = rank . collate

display :: Ranks -> String
display = undefined

collate :: (Codes, Marks) -> [(Name, Mark)]
collate = sortby name . remove . zipp . cross (sortby iden, id)
  where name = fst
        iden = snd
    
rank :: [(Name, Mark)] -> Ranks
rank = undefined

zipp = uncurry zip

remove :: [((Name, Iden), (Iden, Mark))] -> [(Name, Mark)]
remove = map (cross (fst, snd))

sortby :: Ord b => (a -> b) -> [a] -> [a]
sortby = undefined

collate' = sortby name . zipp . cross (map fst . sortby iden, map snd)
  where name = fst
        iden = snd
