module GameLogic where 

import Data.List (transpose)

-- THINGS FOR TESTING PURPOSES

brd = let
  c1 = []
  c2 = [   Red   ,  Yellow  ,   Yellow  ,   Red   , Yellow  ] 
  c3 = [  Yellow ,   Red    ,   Yellow  ,   Red   , Yellow  ]
  c4 = [   Red   ,  Yellow  ]
  c5 = [   Red   ,   Yellow ,   Yellow  ]
  c6 = [  Yellow ,    Red   ,   Yellow  ,   Red   ]
  c7 = [   Red   ,   Yellow ,    Red    ,   Red    ,   Red  ,  Yellow]
  in
  [c1, c2, c3, c4, c5, c6, c7]

st = BS {
        theBoard = brd,
        lastMove = Red,
        numColumns = 7,
        numRows = 6,
        numToConnect = 4
        }


data Piece = Red | Yellow 
    deriving(Eq)

instance Show Piece where
    show Red = "R"
    show Yellow = "Y"


type Column = [Piece]
type Board = [Column]

data BoardState = BS 
  {
      theBoard :: Board,
      lastMove :: Piece,
      numColumns :: Int,
      numRows :: Int,
      numToConnect :: Int
  } deriving(Show, Eq)

opposite :: Piece -> Piece 
opposite Red = Yellow 
opposite Yellow = Red

makeMove :: BoardState -> Int -> Maybe BoardState
makeMove bs dropCol = 
    if dropCol < 1 || dropCol > (numColumns bs)
        then Nothing -- check that the drop is into a valid column 
        else 
            case selectNth dropCol (theBoard bs) of
                (colsBefore,workingCol,colsAfter) ->
                    let newWorkingCol = workingCol ++ [opposite (lastMove bs)] 
                    in 
                        if length newWorkingCol > numRows bs
                            then Nothing -- check that the drop into a column doesn't overfill 
                            else Just $ bs {theBoard = colsBefore ++ (newWorkingCol:colsAfter),lastMove = opposite (lastMove bs)}

{-
Precondition: xs must have an nth element.
Note, we index from 1.  so
    select 1 [x1,x2,x3,...,xn] = ([],x1,[x2,...])
More generally
    select k [x1,...,xn] = ([x1,...,x(k-1)],xk,[x(k+1),...,xn])
i.e.
    select k xs = (ys,x,zs) such that xs == ys ++ x:zs
-}
selectNth :: Int -> [a] -> ([a],a,[a])
selectNth n xs = 
    let
        (ys,x:zs) = splitAt (n-1) xs 
    in 
        (ys,x,zs)

checkWin :: BoardState -> Maybe Piece
checkWin bs = 
    let
        connectionNumber = numToConnect bs 
        winRows = connectAny connectionNumber (rows bs)
        winCols = connectAny connectionNumber (columns bs)
        winDiagForward = connectAny connectionNumber (diagonalsForward bs)
        winDiagBackward = connectAny connectionNumber (diagonalsBackward bs)
    in
        winRows `plusMaybe` winCols `plusMaybe` winDiagForward `plusMaybe` winDiagBackward

connectAny :: Int -> [[Maybe Piece]] -> Maybe Piece
connectAny n = foldr plusMaybe Nothing . map (connect n)

connect :: Int -> [Maybe Piece] -> Maybe Piece 
connect n [] = Nothing
connect n (x:xs) = connectNCol n 1 x xs

connectNCol :: Int -> Int -> Maybe Piece -> [Maybe Piece] -> Maybe Piece
connectNCol connNum connSoFar lastCol [] = if connNum == connSoFar then lastCol else Nothing    
connectNCol connNum connSoFar lastCol (x:xs) = 
    if connNum == connSoFar 
        then lastCol
        else
            case (lastCol,x) of
                (Nothing,Nothing) -> connectNCol connNum 0 Nothing xs
                (Nothing,Just Red) -> connectNCol connNum 1 (Just Red) xs 
                (Nothing,Just Yellow) -> connectNCol connNum 1 (Just Yellow) xs 
                (Just Red,Nothing) -> connectNCol connNum 0 Nothing xs 
                (Just Red,Just Red) -> connectNCol connNum (connSoFar + 1) (Just Red) xs 
                (Just Red,Just Yellow) -> connectNCol connNum 1 (Just Yellow) xs 
                (Just Yellow,Nothing) -> connectNCol connNum 0 Nothing xs 
                (Just Yellow,Just Red) -> connectNCol connNum 1 (Just Red) xs 
                (Just Yellow,Just Yellow) -> connectNCol connNum (connSoFar + 1) (Just Yellow) xs
        
{-
Usually one would use "mplus" but we haven't gotten to MonadPlus.
-}
plusMaybe :: Maybe a -> Maybe a -> Maybe a
plusMaybe Nothing x = x
plusMaybe (Just y) x = Just y

rows :: BoardState -> [[Maybe Piece]]
rows = reverse . transpose . columns

columns :: BoardState -> [[Maybe Piece]]
columns bs = map (padN (numRows bs)) (theBoard bs)

padN :: Int -> [a] -> [Maybe a]
padN k [] = take k (repeat Nothing)
padN k (x:xs) = Just x : padN (k-1) xs

{-
Precondition: In order for this function to work properly,
the list input must be a perfectly rectangular list.  In other words
it must be a list of lists where each element has the same length.

The idea of this function: 
  m11 m12 m13 m14
  m21 m22 m23 m24
  m31 m32 m33 m34
  m41 m42 m43 m44

If we extract the first row:
   m11 m12 m13 m14 
We take m11 and put it in completely processed.
We then take 
   m12 m13 m14
and pair it with
   m21 m22 m23 with m24 left over.
We then get
   [[m21,m12],[m22,m13],[m23,m14],[m24]]
We then take the head and put it onto completelyProcessed so that completely processed is
  [[m11],[m21,m12]]
One can see this inductively keeps going!!
splitDiagonals completelyProcessed currentGen unProcessed
-}
splitDiagonals :: [[a]] -> [[a]] -> [[a]] -> [[a]]
splitDiagonals completelyProcessed finale [] = completelyProcessed ++ finale
splitDiagonals completelyProcessed (toBeEnqueued:currentGen) (nextRow:unProcessed) =
    splitDiagonals (completelyProcessed ++ [toBeEnqueued]) (pairShuffled currentGen nextRow) unProcessed

{-
Precondition: length of list1 is is length of list2 - 1
-}
pairShuffled :: [[a]] -> [a] -> [[a]]
pairShuffled [] newElement = [newElement]
pairShuffled (row:rows) (x:xs) = (x:row) : pairShuffled rows xs


{-
To use splitDiagonals to do diagonalsForward
We first take the first row, and 

-}
diagonalsForward :: BoardState -> [[Maybe Piece]]
diagonalsForward = pureDiagonals . rows 

{-
Precondition: pureDiagonals requires a non-empty list.
-}
pureDiagonals :: [[a]] -> [[a]] 
pureDiagonals (firstRow:restOfTheRows) = 
    splitDiagonals [] (map (\x -> [x]) firstRow) restOfTheRows

{-
If we rotate the board to the right and collect the diagonals, we will have collected all
the diagonals, but each diagonal will have been collected in the reverse order.  Hence
we map reverse onto each diagonal.

E.g.

if we have
    m11 m12
    m21 m22 
and we rotate right we have 
    m21 m11 
    m22 m12 
if we collect the diagonals here, we have 
    [[m21],[m22,m11],[m12]]
so that we have travelled down each diagonal, but we travelled the diagonals bottom right to top left
when we wanted top left to bottom right.  Thus we simply reverse each diagonal to get
   [[m21],[m11,m22],[m12]]
which is correct.
-}
diagonalsBackward :: BoardState -> [[Maybe Piece]]
diagonalsBackward = map reverse . pureDiagonals . rotateRight . rows

rotateRight :: [[a]] -> [[a]]
rotateRight = (map reverse) . transpose 

egBoard :: BoardState
egBoard = BS {
    theBoard = [[Yellow],[Red],[Yellow,Yellow],[Red],[]],
    numRows = 4,
    numColumns = 5,
    numToConnect = 3,
    lastMove = Yellow
  }

{-}
suggestMove :: BoardState -> Maybe Int
suggestMove bs = if (checkBoardFull bs) then Nothing
                 else  -- board is not full
                     if (checkIfAnyMoveIsAWin bs == Nothing) then -- there is no winning move
                         Just 3
                     else  -- there is a winning move
                         checkIfAnyMoveIsAWin bs  -- return the winning move
-}

-- returns Nothing if there is no winning move, Just <columns of winning move> if there is a winning move
checkIfAnyMoveIsAWin :: BoardState -> Maybe Int
checkIfAnyMoveIsAWin bs = ciamiawHelper (bs, numColumns bs)


-- ciamiawHelper returns Just <column of winning move> if there is one, Nothing is there is no winning move
ciamiawHelper(bs, 0) = Nothing -- reached column 0, there is no move that is a win
ciamiawHelper(bs, currentCol) = if ((makeMove bs currentCol) == Nothing) then
                                   ciamiawHelper(bs, (currentCol - 1)) -- if the curretCol is full, check next col
                                else
                                  let
                                    colorPlaying = oppositePiece (lastMove bs)
                                    modifiedBoard = unJust (makeMove bs currentCol)
                                    winner = checkWin modifiedBoard
                                  in
                                    if (winner == Just colorPlaying) then
                                      (Just currentCol)
                                    else
                                      ciamiawHelper(bs, (currentCol - 1))

-- returns Nothing if not a win, Just col if dropping in col is a win
checkIfMoveIsAWin :: BoardState -> Int -> Maybe Int
checkIfMoveIsAWin bs col = if ((makeMove bs col) == Nothing) then Nothing else -- return nothing if the columns you are trying to drop in is full
                           let
                             colorPlaying = oppositePiece (lastMove bs)
                             modifiedBoard = unJust (makeMove bs col)
                             winner = checkWin modifiedBoard
                           in
                             if (winner == Just colorPlaying) then (Just col) else Nothing


--removes Just
unJust :: Maybe a -> a
unJust (Just x) = x

checkBoardFull :: BoardState -> Bool
checkBoardFull bs = 
    let 
        cols = theBoard bs
    in
        all (\col -> length col == numRows bs) cols 


oppositePiece :: Piece -> Piece
oppositePiece Yellow = Red
oppositePiece Red = Yellow