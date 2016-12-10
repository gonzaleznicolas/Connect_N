module GameLogic where 

import Data.List (transpose)
import System.Random

-- THINGS FOR TESTING PURPOSES

brd = let
  c1 = [Yellow,Yellow,Yellow, Red] --  Red   ,   Red    ,     Red   ,   Red   , Yellow  ]
  c2 = [Yellow,Yellow,Yellow,Yellow] --   Red   ,  Yellow  ,   Yellow  ,   Red   , Yellow  ] 
  c3 = [Red,Yellow,Yellow,Yellow] --  Yellow ,   Red    ,   Yellow  ,   Red   , Yellow  ]
  c4 = [Yellow,Yellow,Yellow,Yellow] --   Red   ,  Yellow  ]
  c5 = [Yellow,Yellow,Yellow,Yellow] --   Red   ,   Yellow ,    Red    ]
  c6 = [Red,Yellow,Yellow] --  Yellow ,    Red   ,   Yellow  ]
  c7 = [Yellow,Yellow,Yellow,Yellow]
  in
  [c1, c2, c3, c4, c5, c6, c7]

st = BS {
        theBoard = brd,
        lastMove = Red,
        numColumns = 7,
        numRows = 4,
        numToConnect = 5
        }

brd1 = let
  c1 = [   Red   ]
  c2 = [   Red   ,  Yellow  ,   Yellow  ,   Red   , Yellow  ] 
  c3 = [  Yellow ,   Red    ,   Yellow  ,   Red   , Yellow  ]
  c4 = [   Red   ,  Yellow  ,   Yellow  , Yellow  ]
  c5 = [   Red   ,   Yellow ,   Green    ]
  c6 = [  Yellow ,    Red   ,    Red    ]
  c7 = [  Yellow ,   Yellow ,   Yellow  ,  Red    ,   Red   ,  Yellow]
  in
  [c1, c2, c3, c4, c5, c6, c7]

st1 = BS {
        theBoard = brd1,
        lastMove = Red,
        numColumns = 7,
        numRows = 6,
        numToConnect = 4
        }

data Piece = Red | Yellow | Green
    deriving(Eq)

instance Show Piece where
    show Red = "R"
    show Yellow = "Y"
    show Green = "G"



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


suggestMove :: BoardState -> Maybe Int
suggestMove bs = if (myCheckBoardFull bs) then Nothing
                 else
                     suggestMoveHelper (bs, 0, numToConnect bs)

--precondition: board is not full
suggestMoveHelper :: (BoardState, Int, Int) -> Maybe Int
suggestMoveHelper (bs, sub, n) = if (sub >= (n - 1)) then  -- if we are just trying to find a place to drop that completes 1 in a row, then just place it anywhere where there is space
                                     selectMove bs
                                 else
                                     if (checkIfAnyMoveIsAWin (decrementNumToConnect bs sub) == Nothing) then
                                         -- if here, it was not possible to connect n-sub, see if the opponent can connect n-sub
                                         if (checkIfOpponentCanWin (decrementNumToConnect bs sub) == Nothing) then
                                             -- if here, not you and not your opponent can make a move to connect n-sub. So check for n-(sub+1)
                                             suggestMoveHelper(bs, (sub + 1), n)
                                         else 
                                            -- if here, your opponent can connect n-sub. block it
                                            checkIfOpponentCanWin (decrementNumToConnect bs sub)
                                     else
                                         -- if here, you can connect n-sub. Do it.
                                         checkIfAnyMoveIsAWin (decrementNumToConnect bs sub)




-- returns Just num of a column that is not full. Nothing if all are full
selectMove :: BoardState -> Maybe Int
selectMove bs = let
                   startingCol = if (numColumns bs)-3 >= 1 then
                                    ((numColumns bs)-3)
                                 else
                                     if (numColumns bs)-2 >= 1 then
                                        ((numColumns bs)-2)
                                     else 
                                        if (numColumns bs)-2 >= 1 then 
                                            ((numColumns bs)-2)
                                        else (numColumns bs)
                in
                   selectMoveHelper(bs, startingCol, numColumns bs)


selectMoveHelper :: (BoardState, Int, Int) -> Maybe Int
selectMoveHelper(bs, currentCol, 0) = Nothing
selectMoveHelper(bs, 0, numTries) = selectMoveHelper(bs, numColumns bs, numTries)
selectMoveHelper(bs, currentCol, numTries) = if ((makeMove bs currentCol) == Nothing) then
                                   selectMoveHelper(bs, (currentCol - 1), (numTries - 1)) -- if the currentCol is full, check next col
                                else
                                   Just currentCol


checkIfOpponentCanWin :: BoardState -> Maybe Int
checkIfOpponentCanWin bs = ciamiawHelper (flipLastMove bs, numColumns bs)


flipLastMove :: BoardState -> BoardState
flipLastMove bs = BS {
            theBoard = theBoard bs,
            lastMove = oppositePiece (lastMove bs),
            numColumns = numColumns bs,
            numRows = numRows bs,
            numToConnect = numToConnect bs
            }

decrementNumToConnect :: BoardState -> Int -> BoardState
decrementNumToConnect bs n = BS {
            theBoard = theBoard bs,
            lastMove = lastMove bs,
            numColumns = numColumns bs,
            numRows = numRows bs,
            numToConnect = ((numToConnect bs) - n)
            }

setNumToConnect :: BoardState -> Int -> BoardState
setNumToConnect bs n = BS {
            theBoard = theBoard bs,
            lastMove = lastMove bs,
            numColumns = numColumns bs,
            numRows = numRows bs,
            numToConnect = n
            }

-- returns Nothing if there is no winning move, Just <columns of winning move> if there is a winning move
checkIfAnyMoveIsAWin :: BoardState -> Maybe Int
checkIfAnyMoveIsAWin bs = ciamiawHelper (bs, numColumns bs)


-- ciamiawHelper returns Just <column of winning move> if there is one, Nothing is there is no winning move
ciamiawHelper :: (BoardState, Int) -> Maybe Int
ciamiawHelper(bs, 0) = Nothing -- reached column 0, there is no move that is a win
ciamiawHelper(bs, currentCol) = if ((myMakeMove bs currentCol Green) == Nothing) then
                                   ciamiawHelper(bs, (currentCol - 1)) -- if the curretCol is full, check next col
                                else
                                  let
                                    colorPlaying = oppositePiece (lastMove bs)
                                    modifiedBoard = unJust (myMakeMove bs currentCol Green)
                                    winner = myCheckWin modifiedBoard colorPlaying
                                  in
                                    if (winner == Just colorPlaying) then
                                      (Just currentCol)
                                    else
                                      ciamiawHelper(bs, (currentCol - 1))



--removes Just
unJust :: Maybe a -> a
unJust (Just x) = x

myCheckBoardFull :: BoardState -> Bool
myCheckBoardFull bs = 
    let 
        cols = theBoard bs
    in
        all (\col -> length col == numRows bs) cols 


oppositePiece :: Piece -> Piece
oppositePiece Yellow = Red
oppositePiece Red = Yellow


-- returns Just colorInQuestion if colorInQuestion won and its winning streak contains exactly 1 Green anywhere in it. Returns Nothing otherwise.
myCheckWin :: BoardState -> Piece -> Maybe Piece
myCheckWin st colorInQuestion = let
                n = numToConnect st
                columnsWin = twoDarrayWinChecker(colorInQuestion, n, columns st, False)
                rowsWin = twoDarrayWinChecker(colorInQuestion, n, rows st, False)
                diagForwardWin = twoDarrayWinChecker(colorInQuestion, n, diagonalsForward st, False)
                diagBackwardWin = twoDarrayWinChecker(colorInQuestion, n, diagonalsBackward st, False)
              in
                if (columnsWin || rowsWin || diagForwardWin || diagBackwardWin) then Just colorInQuestion else Nothing

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f n [] = n
myfoldr f n (x:l) = f x (foldr f n l)


-- this function takes in a piece (Yellow or Red) and an array [Maybe Piece].
-- It returns true if array contains n consecutive pieces of color. Otherwise it returns false.
-- the count must be initially 0
-- winFlag must be initially false
arrayWinChecker :: (Piece, Int, Int, [Maybe Piece], Bool) -> Bool
arrayWinChecker (color, n, count, list , winFlag) = myCheckWinHelper (color, n, count, (list ++ [Nothing]), winFlag, False)




-- this function takes in a piece (Yellow or Red) and an array [Maybe Piece].
-- It returns true if array contains n consecutive pieces of color (exception: exactly one has to be green). Otherwise it returns false.
-- the count must be initially 0
-- winFlag must be initially false
myCheckWinHelper :: (Piece, Int, Int, [Maybe Piece], Bool, Bool) -> Bool
myCheckWinHelper (color, n, count, [], winFlag, greenFlag) = winFlag -- since the array is empty, we are done reading the array
myCheckWinHelper (color, n, count, x:xs, winFlag, greenFlag) = if ((count == n) && greenFlag == True)
                                                              then True
                                                              else( -- in the reading we have done so far, no player has won
                                                                 if (x == (Just Green))
                                                                 then (myCheckWinHelper (color, n, count+1, xs, False, True))
                                                                 else
                                                                   if (x == (Just color)) -- read a piece of the color we are interested in
                                                                   then (myCheckWinHelper (color, n, count+1, xs, False, greenFlag))
                                                                   else (myCheckWinHelper (color, n, 0, xs, False, False)))



{-

myCheckWinHelper :: (Piece, Int, Int, [Maybe Piece], Bool, Bool) -> Bool
myCheckWinHelper (color, n, count, [], winFlag, b) = winFlag -- since the array is empty, we are done reading the array
myCheckWinHelper (color, n, count, x:xs, winFlag, b) = if (count == n) --player won
                                                  then True
                                                  else( -- in the reading we have done so far, no player has won
                                                     if (x == (Just color)) -- read a piece of the color we are interested in
                                                     then (myCheckWinHelper (color, n, count+1, xs, False, b))
                                                     else (myCheckWinHelper (color, n, 0, xs, False, b)))

-}


-- this function takes a piece (Yellow or red), a number n (number of consecutive pieces neded to win),
-- a 2d array [[Maybe Piece]], and a winFlag which must initially be false
-- twoDarrayWinChecker returns true if in any of the subarrays, there are n concecutive piece. False otherwise
twoDarrayWinChecker :: (Piece, Int, [[Maybe Piece]], Bool) -> Bool
twoDarrayWinChecker (color, n, ary, True) = True -- if any subarray found a win, return win
twoDarrayWinChecker (color, n, [], winFlag) = False -- if the array is empty, we have looked through all the subarrays and found no wins. return false
twoDarrayWinChecker (color, n, x:xs, False) = twoDarrayWinChecker (color, n, xs, arrayWinChecker (color, n, 0, x, False))


-- the Int is the column (column numbering starts at 1) into which we want to drop a piece of color color
myMakeMove :: BoardState -> Int -> Piece -> Maybe BoardState
myMakeMove state n color = if ((length columnInQuestion) == numRows state) then Nothing else Just $
            BS {
            theBoard = newBoard,
            lastMove = newPiece,
            numColumns = (numColumns state),
            numRows = (numRows state),
            numToConnect = (numToConnect state)
            }
    where
        -- "n-1" in the lines below is because replaceElemAt and elemAt assume indexing starts at zero. But columns are numbered starting with 1.
        columnInQuestion = elemAt (n-1) (theBoard state)
        updatedColumn = columnInQuestion ++ [color]
        newBoard = replaceElemAt (n-1) (theBoard state) updatedColumn
        newPiece = oppositePiece (lastMove state)


-- replaces the element in list l at index n with element. indexes start at 0.
replaceElemAt:: Int -> [a] -> a -> [a]
replaceElemAt n l element =(fst (splitAt n l) ++ [element]) ++ (tail (snd (splitAt n l )))

-- elemAt defined in lecture. takes and index and a list and returns the element at that index. indexes start at 0.
elemAt :: Int -> [a] -> a
elemAt 0 (x:xs) = x
elemAt n (x:xs) = elemAt (n-1) xs