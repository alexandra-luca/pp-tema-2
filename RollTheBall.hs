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

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = SpatiuGol | Bloc | Tub Directions Directions | Start Directions | Finish Directions
    deriving (Eq, Ord)
instance Show Cell where
    show SpatiuGol = [emptySpace]
    show Bloc = [emptyCell]
    show (Tub West East) = [horPipe]
    show (Tub East West) = [horPipe]
    show (Tub North South) = [verPipe]
    show (Tub South North) = [verPipe]
    show (Tub South East) = [topLeft]
    show (Tub East South) = [topLeft]
    show (Tub North East) = [botLeft]
    show (Tub East North) = [botLeft]
    show (Tub West North) = [botRight]
    show (Tub North West) = [botRight]
    show (Tub West South) = [topRight]
    show (Tub South West) = [topRight]
    show (Start North) = [startUp]
    show (Start South) = [startDown]
    show (Start West) = [startLeft]
    show (Start East) = [startRight]
    show (Finish North) = [winUp]
    show (Finish South) = [winDown]
    show (Finish West) = [winLeft]
    show (Finish East) = [winRight]

charToCell :: Char -> Cell
charToCell c =
    if c == emptySpace then SpatiuGol else
    if c == emptyCell then Bloc else
    if c == horPipe then (Tub West East) else
    if c == horPipe then (Tub East West) else
    if c == verPipe then (Tub North South) else
    if c == verPipe then (Tub South North) else
    if c == topLeft then (Tub South East) else
    if c == topLeft then (Tub East South) else
    if c == botLeft then (Tub North East) else
    if c == botLeft then (Tub East North) else
    if c == botRight then (Tub West North) else
    if c == botRight then (Tub North West) else
    if c == topRight then (Tub West South) else
    if c == topRight then (Tub South West) else
    if c == startUp then (Start North) else
    if c == startDown then (Start South) else
    if c == startLeft then (Start West) else
    if c == startRight then (Start East) else
    if c == winUp then (Finish North) else
    if c == winDown then (Finish South) else
    if c == winLeft then (Finish West) else
    if c == winRight then (Finish East) else SpatiuGol


isFinish (Finish _) = True
isFinish _ = False
isTube (Tub _ _) = True
isTube _ = False
isStart (Start _) = True
isStart _ = False


{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level (A.Array (Int, Int) Cell)
    deriving (Eq, Ord)
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

getArrMaxLine arr = fst (snd (A.bounds arr))
getArrMaxCol arr = snd (snd (A.bounds arr))
printArrLine arr i = (foldl (++) [] [show (arr A.! (i,j)) | j <- [0..(getArrMaxCol arr)]]) ++ [endl]

instance Show Level 
    where show (Level arr) = [endl] ++ (foldl (++) [] [printArrLine arr i | i <- [0..getArrMaxLine arr]])

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel (x,y) = Level (A.array((0,0), (x,y)) 
                         [((i,j), SpatiuGol) | i <- [0..x], j <- [0..y]])

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


isValidAndEmpty :: Position -> Level -> Bool
isValidAndEmpty (i, j) (Level arr) = 
    if (i < 0) || (j < 0) || (i > (getArrMaxLine arr)) || (j > (getArrMaxCol arr)) then False
        else if arr A.! (i, j) == SpatiuGol then True else False


addCell :: (Char, Position) -> Level -> Level
addCell (char, pos) (Level arr) = if isValidAndEmpty pos (Level arr) then Level (arr A.// [(pos, (charToCell char))]) else Level arr


{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel dreaptajos lista_de_celule = foldl (\x y -> addCell y x) (emptyLevel dreaptajos) lista_de_celule


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

getAdjacentPosition :: Position -> Directions -> Position
getAdjacentPosition (i,j) North = (i-1, j)
getAdjacentPosition (i,j) South = (i+1, j)
getAdjacentPosition (i,j) West = (i, j-1)
getAdjacentPosition (i,j) East = (i, j+1)

moveCell :: Position -> Directions -> Level -> Level
moveCell pos dir (Level arr) = 
    if (isStart (arr A.! pos)) || (isFinish (arr A.! pos)) then (Level arr)
        else
            if (isValidAndEmpty adjpos (Level arr)) then
                Level (arr A.// [(pos, SpatiuGol), (adjpos, arr A.! pos)])
                else Level arr
    where adjpos = getAdjacentPosition pos dir

{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}

directiaOpusa :: Directions -> Directions
directiaOpusa North = South
directiaOpusa South = North
directiaOpusa West = East
directiaOpusa East = West

connection :: Cell -> Cell -> Directions -> Bool
connection (Tub dir1 dir2) (Tub dir3 dir4) dir =
    (dir1 == dir || dir2 == dir) && (dir3 == dirop || dir4 == dirop) 
    where dirop = directiaOpusa dir
connection (Tub dir1 dir2) (Start dir3) dir =
    (dir1 == dir || dir2 == dir) && (dir3 == dirop)
    where dirop = directiaOpusa dir
connection (Tub dir1 dir2) (Finish dir3) dir =
    (dir1 == dir || dir2 == dir) && (dir3 == dirop)
    where dirop = directiaOpusa dir
connection (Start dir1) (Tub dir3 dir4) dir =
    (dir1 == dir) && (dir3 == dirop || dir4 == dirop)
    where dirop = directiaOpusa dir
connection (Finish dir1) (Tub dir3 dir4) dir =
    (dir1 == dir) && (dir3 == dirop || dir4 == dirop)
    where dirop = directiaOpusa dir
connection (Start dir1) (Finish dir3) dir =
    (dir1 == dir) && (dir3 == dirop)
    where dirop = directiaOpusa dir
connection (Finish dir1) (Start dir3) dir =
    (dir1 == dir) && (dir3 == dirop)
    where dirop = directiaOpusa dir
connection _ _ _ = False

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

directiaCealaltaDecatAiaDinCareAmVenit dir (Tub dir1 dir2) = 
    if (dir == dir1) then dir2 else dir1

wonL :: Level -> Position -> Directions -> Bool
wonL (Level arr) pos dir_initiala = 
    if isFinish currentcell then True
    else if isTube currentcell then
            let 
                dircealalta = directiaCealaltaDecatAiaDinCareAmVenit dir_initiala currentcell
                adjpos = getAdjacentPosition pos dircealalta
                adjcell = (arr A.! adjpos)
            in
                (connection currentcell adjcell dircealalta) && wonL (Level arr) adjpos (directiaOpusa dircealalta)
    else False
    where currentcell = (arr A.! pos)

findStartPosition :: [(Position, Cell)] -> Position
findStartPosition (h:t) =
    if isStart (snd h) then fst h else findStartPosition t

getStartDir (Start dir) = dir

wonLevel :: Level -> Bool
wonLevel (Level arr) =
    let startpos = findStartPosition (assocs arr) 
        startdir = getStartDir (arr A.! startpos)
        adjpos = getAdjacentPosition startpos startdir
        adjcell = (arr A.! adjpos)
    in
        (connection (arr A.! startpos) adjcell startdir) && (wonL (Level arr) adjpos (directiaOpusa startdir))

instance ProblemState Level (Position, Directions) where
    successors = undefined
    isGoal = undefined
    reverseAction = undefined
