{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.List

import qualified Data.Array as A 




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
arrt = A.array ((0, 0), (2, 2)) [
    ((0, 0), (Cell startDown)), ((0, 1), (Cell emptySpace)), ((0, 2), (Cell emptySpace)),
    ((1, 0), (Cell verPipe)), ((1, 1), (Cell emptySpace)), ((1, 2), (Cell emptySpace)),
    ((2, 0), (Cell winUp)), ((2, 1), (Cell emptySpace)), ((2, 2), (Cell emptySpace))]
rar =  A.array ((0, 0), (1, 1)) [((0,0), (Cell emptySpace)), ((0,1), (Cell emptySpace)), ((1, 0), (Cell startUp)), ((1,1), (Cell emptySpace))]
arr4sol = A.array ((0, 0), (3, 3)) [
        ((0, 0), (Cell startDown)), ((0, 1), (Cell emptyCell)), ((0, 2), (Cell emptyCell)), ((0, 3), (Cell emptyCell)), 
        ((1, 0), (Cell verPipe)), ((1, 1), (Cell emptyCell)), ((1, 2), (Cell emptyCell)), ((1, 3), (Cell emptyCell)), 
        ((2, 0), (Cell verPipe)), ((2, 1), (Cell emptySpace)), ((2, 2), (Cell emptyCell)), ((2, 3), (Cell emptyCell)), 
        ((3, 0), (Cell botLeft)), ((3, 1), (Cell horPipe)), ((3, 2), (Cell horPipe)), ((3, 3), (Cell winLeft))]

arr4 = A.array ((0, 0), (3, 3)) [
        ((0, 0), (Cell startDown)), ((0, 1), (Cell emptyCell)), ((0, 2), (Cell emptyCell)), ((0, 3), (Cell emptyCell)), 
        ((1, 0), (Cell verPipe)), ((1, 1), (Cell emptyCell)), ((1, 2), (Cell emptyCell)), ((1, 3), (Cell emptyCell)), 
        ((2, 0), (Cell verPipe)), ((2, 1), (Cell horPipe)), ((2, 2), (Cell emptyCell)), ((2, 3), (Cell emptyCell)), 
        ((3, 0), (Cell botLeft)), ((3, 1), (Cell emptySpace)), ((3, 2), (Cell horPipe)), ((3, 3), (Cell winLeft))]

arr5 = A.array ((0, 0), (3, 3)) [
        ((0, 0), (Cell startDown)), ((0, 1), (Cell emptyCell)), ((0, 2), (Cell emptyCell)), ((0, 3), (Cell emptyCell)),
        ((1, 0), (Cell emptySpace)), ((1, 1), (Cell verPipe)), ((1, 2), (Cell emptySpace)), ((1, 3), (Cell emptyCell)),
        ((2, 0), (Cell verPipe)), ((2, 1), (Cell emptyCell)), ((2, 2), (Cell horPipe)), ((2, 3), (Cell emptySpace)), 
        ((3, 0), (Cell botLeft)), ((3, 1), (Cell emptySpace)), ((3, 2), (Cell horPipe)), ((3, 3), (Cell winLeft))]
arr6 = A.array ((0, 0), (3, 3)) [
        ((0, 0), (Cell startDown)), ((0, 1), (Cell emptyCell)), ((0, 2), (Cell emptyCell)), ((0, 3), (Cell emptyCell)),
        ((1, 0), (Cell verPipe)), ((1, 1), (Cell emptyCell)), ((1, 2), (Cell emptyCell)), ((1, 3), (Cell winDown)),
        ((2, 0), (Cell botLeft)), ((2, 1), (Cell horPipe)), ((2, 2), (Cell botRight)), ((2, 3), (Cell emptyCell)), 
        ((3, 0), (Cell emptySpace)), ((3, 1), (Cell emptySpace)), ((3, 2), (Cell emptySpace)), ((3, 3), (Cell horPipe))]


lv2 = Level rar
lv3 = Level arrt
lv4sol = Level arr4sol
lv4 = Level arr4
lv5 = Level arr5
lv6 = Level arr6

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

connectToRight :: [Char]
connectToRight = [horPipe, botRight, topRight, winLeft, startLeft]

connectToLeft :: [Char]
connectToLeft = [horPipe, topLeft, botLeft, winRight, startRight]

connectToUp :: [Char]
connectToUp = [verPipe, topRight, topLeft, winDown, startDown]

connectToDown :: [Char]
connectToDown = [verPipe, botRight, botLeft, winUp, startUp]



getConnectionType :: Directions -> [Char]
getConnectionType d
                    | d == North = connectToUp
                    | d == South = connectToDown
                    | d == East  = connectToRight
                    | otherwise = connectToLeft

getOpositeDirection :: Directions -> Directions
getOpositeDirection d 
                    | d == East = West
                    | d == West = East
                    | d == North = South
                    | otherwise = North

connection :: Cell -> Cell -> Directions -> Bool
connection cell1@(Cell cg1) cell2@(Cell cg2) dir =  (inArray arrayToConnect cg2) && (inArray arrayToConnectOpposite cg1)
                                                    where
                                                    arrayToConnect = getConnectionType dir
                                                    opDir = getOpositeDirection dir
                                                    arrayToConnectOpposite = getConnectionType opDir

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

justExtractInt (Just c) = c
 
getStartPossition :: Level -> Position
getStartPossition lv@(Level arr)  =     (toInteger line, toInteger  col)
                                        where
                                        elements_as_cells = A.elems arr
                                        elements = map category elements_as_cells
                                        just_index = head $ filter (/= Nothing) [ elemIndex startType elements | startType <- startCells ]
                                        index  = justExtractInt just_index
                                        dimm = snd (A.bounds arr)
                                        width = snd (snd (A.bounds arr)) + 1
                                        line = index `div` (fromInteger width)
                                        col  = index `mod` (fromInteger width) 


help :: Level -> Integer
help lv@(Level arr)  =  width
                            where
                            elements_as_cells = A.elems arr
                            elements = map category elements_as_cells
                            just_index = head $ filter (/= Nothing) [ elemIndex startType elements | startType <- startCells ]
                            index  = justExtractInt just_index
                            dimm = snd (A.bounds arr)
                            width = snd (snd (A.bounds arr)) + 1
                            line = index `div` (fromInteger width)
                            col  = index `mod` (fromInteger width) 


getNextPossibleWays :: Level -> Position -> [(Position, Directions)]
getNextPossibleWays lv@(Level arr) pos@(l, c) 
                                            | cell_category == horPipe = [((l, c - 1), West), ((l, c + 1), East)]
                                            | cell_category == verPipe = [((l + 1, c), South), ((l - 1, c), North)]
                                            | cell_category == topLeft = [((l + 1, c), South), ((l, c + 1), East)]
                                            | cell_category == botLeft = [((l - 1, c), North), ((l, c + 1), East)]
                                            | cell_category == botRight = [((l - 1, c), North), ((l, c - 1), West)]
                                            | cell_category == topRight = [((l + 1, c), South), ((l, c - 1), West)]
                                            | cell_category == startUp = [((l - 1, c), North)]
                                            | cell_category == startDown = [((l + 1, c), South)]
                                            | cell_category == startLeft = [((l, c - 1), West)]
                                            | cell_category == startRight = [((l, c + 1), East)]
                                            | otherwise = [((-1, -1), North)]
                                            where
                                            cell = arr A.! pos
                                            cell_category = category cell



compareWithInitialPos :: Level -> (Position, Directions) -> Bool
compareWithInitialPos lv@(Level arr) ((ll, rr), dd) =   cat /= emptySpace
                                                        where
                                                        cell_elem = arr A.! (ll, rr)
                                                        cat = category cell_elem



getNextToCheck :: Level -> Position -> (Position, Directions)
getNextToCheck lv@(Level arr) pos@(l, c) =  if status
                                            then ((next_line, next_column), next_direction)
                                            else ((-1, -1), North)
                                            where
                                            dimm = snd (A.bounds arr)
                                            width = snd (snd (A.bounds arr))
                                            height = fst (snd (A.bounds arr))
                                            next_data = getNextPossibleWays lv pos
                                            parsed_next_data = filter (compareWithInitialPos lv) next_data
                                            ((next_line, next_column), next_direction) = if parsed_next_data == [] then ((-1, -1), North) else head parsed_next_data
                                            status = (next_line >= 0) && (next_line <= height) && (next_column >= 0) && (next_column <= width)

wonLevelHelper :: Level -> Position -> Bool -> Bool
wonLevelHelper lv@(Level arr) start@(l, c) status = if is_recursion_finished then True
                                                    else if (is_valid_next && is_connected_with_next) then wonLevelHelper next_level (next_line, next_column) status
                                                    else False 
                                                    where
                                                    ((next_line, next_column), next_direction) = getNextToCheck lv start
                                                    is_valid_next = (next_line >= 0) && (next_column >= 0)
                                                    this_cell =  arr A.! start
                                                    next_cell =  arr A.! (next_line, next_column)
                                                    is_connected_with_next = connection this_cell next_cell next_direction
                                                    next_level = addCell (emptySpace, start) lv
                                                    is_recursion_finished = (inArray winningCells (category this_cell))

wonLevel :: Level -> Bool
wonLevel lv@(Level arr) =   wonLevelHelper lv (l, c) True
                            where
                            (l, c) = getStartPossition lv

arrTest = A.array ((0,0), (1, 1)) [((0, 0), 0), ((0, 1), 1), ((1, 0), 2), ((1, 1), 3)]
arrE = A.assocs (cells lv4)
arrC = map (category . snd) arrE




isInLimits::Level -> (Position, Directions) -> Bool
isInLimits lv@(Level arr) ((next_line, next_column), next_direction) = (next_line >= 0) && (next_line <= height) && (next_column >= 0) && (next_column <= width)
                                                    where
                                                    dimm = snd (A.bounds arr)
                                                    width = snd (snd (A.bounds arr))
                                                    height = fst (snd (A.bounds arr))

isElementFromPositionEqualToEmptySpace :: Level -> (Position, Directions) -> Bool
isElementFromPositionEqualToEmptySpace lv@(Level arr) ((l, r), d) = (cg == emptySpace)
                                                                    where
                                                                    cell = arr A.! (l, r)
                                                                    cg = category cell
positionCellToActions :: Level -> (Position, Cell) -> [(Position, Directions)]
positionCellToActions lv@(Level arr) ((l, r), cel@(Cell category)) = validEmptySpace
                                                    where
                                                    possibleDirections = [((l + 1, r), South), ((l - 1, r), North), ((l, r + 1), East), ((l, r - 1), West)]
                                                    validDirections = filter (isInLimits lv) possibleDirections
                                                    validEmptySpace = filter (isElementFromPositionEqualToEmptySpace lv) validDirections


reversePosition :: (Position, Directions) -> (Position, Directions)
reversePosition ((l, r), d) 
                            | d == South = ((l - 1, r), d)
                            | d == North = ((l + 1, r), d)
                            | d == East  = ((l, r - 1), d)
                            | otherwise  = ((l, r + 1), d)


forFilteringStartAndWin :: Level -> ((Position, Directions), Level) -> Bool
forFilteringStartAndWin lv@(Level arr) (((l, r), d), lll) = (notInArray winningCells cg) && (notInArray startCells cg) && (cg /= emptySpace)
                                                            where
                                                            cel = arr A.! (l, r)
                                                            cg = category cel


problemNode :: Level -> (Position, Directions) -> ((Position, Directions), Level)
problemNode lv@(Level arr) ((l, r), d) = (((l, r), d), (moveCell (l, r) d lv))

arr_lev = A.assocs (cells lv4)
all_actions = map (problemNode lv4) $ map reversePosition $ concat $ filter (/= []) (map (positionCellToActions lv4) arr_lev) 


getReversedAction :: (Position, Directions) -> (Position, Directions)
getReversedAction ((l, c), dir)
                                | dir == North = ((l - 1, c), South)
                                | dir == South = ((l + 1, c), North)
                                | dir == East  = ((l, c + 1), West)
                                | otherwise    = ((l, c - 1), East)

-- ((isGoal) .  snd) ((((0, 0), North), lv3): (successors lv3))
-- getOpositeDirection d

instance ProblemState Level (Position, Directions) where
    successors :: Level -> [((Position, Directions), Level)]
    successors lv@(Level arr) = all_actions
                                where
                                level_to_list = A.assocs arr   :: [(Position, Cell)]
                                all_actions = filter (forFilteringStartAndWin lv) $ map (problemNode lv) $ map reversePosition $ concat $ filter (/= []) (map (positionCellToActions lv) level_to_list)
                                



    isGoal::Level -> Bool
    isGoal lv@(Level arr) = wonLevel lv

    reverseAction :: ((Position, Directions), Level) -> ((Position, Directions), Level)
    reverseAction ((pos, dir) , lv) = ((new_pos, new_dir), actionMade) 
                                    where
                                    (new_pos, new_dir) = getReversedAction (pos, dir)
                                    actionMade = moveCell pos dir lv

