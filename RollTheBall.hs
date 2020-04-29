{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A


{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Integer, Integer)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = Cell {
    category :: Char
} deriving (Eq, Ord, Show)

{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level {
    cells :: (A.Array Position Cell)
} deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}
convertToString :: [Cell] -> Integer -> Integer -> String
convertToString elem@(h:t) m c
                    | t == [] =  [(category h)] ++ ['\n']
                    | elem == [] = ['\n']
                    | c == m = (category h) : '\n' : (convertToString t m 0)
                    | otherwise = (category h):(convertToString t m (c + 1))



instance Show Level 
    where show (Level arr) = '\n' : (convertToString elements width 0)
                             where
                             dimm = snd (A.bounds arr)
                             width = snd (snd (A.bounds arr))
                             height = fst (snd (A.bounds arr))
                             elements = A.elems arr :: [Cell]

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}
arrt = A.array ((0, 0), (1, 1)) [((0, 0), (Cell 'z')), ((0, 1), (Cell emptyCell)), ((1, 0), (Cell 'd')), ((1, 1), (Cell 'r'))]

emptyLevel :: Position -> Level
emptyLevel (l, r) = Level $ A.array ((0, 0), (l, r)) [((x, y), (Cell emptySpace)) | x <- [0..l], y <- [0..r]]


{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

addCell :: (Char, Position) -> Level -> Level
addCell (category, (l, r)) oldLevel@(Level arr) = if status
                                                  then lv
                                                  else oldLevel 
                                                    where
                                                    lv = Level (arr A.// [((l, r), (Cell category))])
                                                    dimm = snd (A.bounds arr)
                                                    width = snd (snd (A.bounds arr))
                                                    height = fst (snd (A.bounds arr))
                                                    status = (l >= 0) && (l <= height) && (r >= 0) && (r <= width)
{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}

addAllCells :: Level -> [(Char, Position)]  -> Level
addAllCells clearLevel cells
                            | cells == [] = clearLevel
                            | otherwise = (addAllCells (addCell (head cells) clearLevel) (tail cells))

createLevel :: Position -> [(Char, Position)] -> Level
createLevel rightCorner@(height, width) cells = addAllCells (emptyLevel rightCorner) cells

{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

inArray :: [Char] -> Char -> Bool
inArray array elem = (length [x | x <- array, x == elem]) > 0

notInArray :: [Char] -> Char -> Bool
notInArray array elem = not (inArray array elem)

getIndexByDirection :: Position -> Directions -> Position
getIndexByDirection (l, c) p
                            | p == North = (l - 1, c)
                            | p == South = (l + 1, c)
                            | p == West  = (l, c - 1)
                            | otherwise  = (l, c + 1)
                            
positionIsCorrect :: Position -> Level -> Bool
positionIsCorrect (l, r) oldLevel@(Level arr) = status
                                                where
                                                dimm = snd (A.bounds arr)
                                                width = snd (snd (A.bounds arr))
                                                height = fst (snd (A.bounds arr))
                                                status = (l >= 0) && (l <= height) && (r >= 0) && (r <= width)

moveCell :: Position -> Directions -> Level -> Level
moveCell pos@(l, c) d level =   if (all_positions_are_correct && next_cell_is_empty_space && this_cell_is_not_start_or_stop)
                                then addCell (next_cell_category, pos) (addCell (this_cell_category, next_pos) level)
                                else level
                                where
                                arr = cells level
                                next_pos = getIndexByDirection pos d
                                all_positions_are_correct = (positionIsCorrect next_pos level) &&  (positionIsCorrect pos level)
                                next_cell = arr A.! next_pos
                                next_cell_category = category next_cell
                                next_cell_is_empty_space = next_cell_category == emptySpace
                                this_cell = arr A.! pos
                                this_cell_category = category this_cell
                                this_cell_is_not_start_or_stop = (notInArray winningCells this_cell_category) && (notInArray startCells this_cell_category)
                                

{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection = undefined

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}
wonLevel :: Level -> Bool
wonLevel = undefined

instance ProblemState Level (Position, Directions) where
    successors = undefined
    isGoal = undefined
    reverseAction = undefined
