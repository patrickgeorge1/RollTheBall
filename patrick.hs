import Data.Array as A

type Position = (Int, Int)


data Cell = Cell {
    category :: Char
} deriving (Eq, Ord, Show)


data Level = Level {
    cells :: (A.Array Position Cell)
} deriving (Eq, Ord)

-- arr::(A.Array (Int, Int) Cell)
arr = A.array ((0, 0), (1, 1)) [((0, 0), (Cell 'u')), ((0, 1), (Cell 'l')), ((1, 0), (Cell 'd')), ((1, 1), (Cell 'r'))]

level = Level arr

instance Show Level 
    where show (Level arr) = show width
                             where
                             width = (A.bounds arr)