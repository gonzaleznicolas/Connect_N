{-
Nicolas Gonzalez
10151261
CPSC 449 Fall 2016
T01, L01
-}

-- THINGS FOR TESTING PURPOSES

brd1 = let
  c1 = []
  c2 = [   Red   ,  Yellow  ,   Yellow  ,   Red   , Yellow  ] 
  c3 = [  Yellow ,   Red    ,   Yellow  , Yellow  , Yellow  ]
  c4 = [   Red   ,  Yellow  ,   Yellow  ,  Red    ]
  c5 = [   Red   ,   Yellow ,   Yellow  ]
  c6 = [  Yellow ,    Red   ,   Yellow  ]
  c7 = [   Red   ,   Yellow ,   Yellow  ,  Red    ,   Red   ,  Yellow]
  in
  [c1, c2, c3, c4, c5, c6, c7]

st1 = BS {
        theBoard = brd1,
        lastMove = Red,
        numColumns = 7,
        numRows = 6,
        numToConnect = 4
        }

brd = let
  c1 = [Yellow,Yellow,Yellow] --  Red   ,   Red    ,     Red   ,   Red   , Yellow  ]
  c2 = [Yellow,Yellow,Yellow,Yellow] --   Red   ,  Yellow  ,   Yellow  ,   Red   , Yellow  ] 
  c3 = [Red,Yellow,Yellow,Yellow] --  Yellow ,   Red    ,   Yellow  ,   Red   , Yellow  ]
  c4 = [Yellow,Yellow,Yellow,Yellow] --   Red   ,  Yellow  ]
  c5 = [Yellow,Yellow,Yellow,Yellow] --   Red   ,   Yellow ,    Red    ]
  c6 = [Red,Yellow,Yellow,Yellow] --  Yellow ,    Red   ,   Yellow  ]
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

-- DATA TYPES AND ALIASES

data Piece = Yellow | Red
    deriving(Eq)

--intance <typeclass> <type> where
instance Show Piece where
   show Yellow = "Y"
   show Red = "R" 


type Column = [Piece]
type Board = [Column]

--this is a record
data BoardState = BS
    {theBoard :: Board,
    lastMove :: Piece,
    numColumns :: Int,
    numRows :: Int,
    numToConnect :: Int} deriving(Show)

-- SIX MAIN FUNCTIONS



-- the Int is the column (column numbering starts at 1) into which we want to drop a piece
makeMove :: BoardState -> Int -> Maybe BoardState
makeMove state n = if ((length columnInQuestion) == numRows state) then Nothing else Just $
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
        updatedColumn = columnInQuestion ++ [oppositePiece (lastMove state)]
        newBoard = replaceElemAt (n-1) (theBoard state) updatedColumn
        newPiece = oppositePiece (lastMove state)


-- returns Just Piece if Piece won. Returns Nothing if no one has won
myCheckWin :: BoardState -> Maybe Piece
myCheckWin st = let
                colorInQuestion = lastMove st
                n = numToConnect st
                columnsWin = twoDarrayWinChecker(colorInQuestion, n, columns st, False)
                rowsWin = twoDarrayWinChecker(colorInQuestion, n, rows st, False)
                diagForwardWin = twoDarrayWinChecker(colorInQuestion, n, diagonalsForward st, False)
                diagBackwardWin = twoDarrayWinChecker(colorInQuestion, n, diagonalsBackward st, False)
              in
                if (columnsWin || rowsWin || diagForwardWin || diagBackwardWin) then Just colorInQuestion else Nothing

-- returns a list of disambiguated columns. The pieces are listed from bottom to top on the column.
-- Nothing represents a spot without a piece in it
columns :: BoardState -> [[Maybe Piece]]
columns st = disambiguateBoard (numRows st) (theBoard st)

-- resurns a list of disambiguated rows. The pieces of an individual row are listed from left to right.
-- the rows are listed with the bottom row first.
rows :: BoardState -> [[Maybe Piece]]
rows st = rowsHelper ((columns (st)), [], numRows st)

diagonalsForward :: BoardState -> [[Maybe Piece]]
diagonalsForward st = myfoldr (\x y -> (oneDiagForward(st, x, [])):y) [] (makeL st)

diagonalsBackward :: BoardState -> [[Maybe Piece]]
diagonalsBackward st = myfoldr (\x y -> (oneDiagBackward(st, x, [])):y) [] (makeLRefl st)

-- HELPER FUNCTIONS

rowsHelper :: ([[Maybe Piece]], [[Maybe Piece]], Int) -> [[Maybe Piece]]
rowsHelper (xs, ys, 0) = ys
rowsHelper (xs, ys, n) = rowsHelper (listOfTails xs, (listOfHeads xs):ys, n-1)

-- elemAt defined in lecture. takes and index and a list and returns the element at that index. indexes start at 0.
elemAt :: Int -> [a] -> a
elemAt 0 (x:xs) = x
elemAt n (x:xs) = elemAt (n-1) xs

oppositePiece :: Piece -> Piece
oppositePiece Yellow = Red
oppositePiece Red = Yellow

-- replaces the element in list l at index n with element. indexes start at 0.
replaceElemAt:: Int -> [a] -> a -> [a]
replaceElemAt n l element =(fst (splitAt n l) ++ [element]) ++ (tail (snd (splitAt n l )))

--removes Just
unJust :: Maybe a -> a
unJust (Just x) = x

--adds "Just"
justify :: a -> Maybe a
justify x = Just x

-- Makes l of length n by filling any missing spots with Nothing
disambiguateList :: Int -> [a] -> [Maybe a]
disambiguateList n l = myReverse (dhelp(l, [], n))

-- g is f's helper
dhelp :: ([a], [Maybe a], Int) -> [Maybe a]
dhelp (l, ys, 0) = ys
dhelp ([], ys, n) = dhelp([], Nothing:ys, n-1)
dhelp (x:xs, ys, n) = dhelp(xs, (Just x):ys, n-1)

-- reverses a list (defined in class)
myReverse :: [a] -> [a]
myReverse l = rev([], l)

-- rev is reverse's helper
rev :: ([a],[a]) -> [a]
rev (xs, []) = xs
rev (xs, (y:ys)) = rev(y:xs, ys)

-- myMap is map defined for lists
myMap1 :: ([a] -> [Maybe a]) -> [[a]] -> [[Maybe a]]
myMap1 f = foldr (\x y -> (f x):y) []


-- disambiguateBoard takes an int n and a 2d array of a's. Outputs a 2D array such that all the inner arrays are of length n (missing spots are filled with Nothing)
{-
eg. suppose n is 4
 [
 [1,2,3]
 [4]
 [5,4]
 [5,6,7,8]
 ]
 becomes
 [
 [Just 1, Just 2, Just 3, Nothing]
 [Just 4, Nothing, Nothing, Nothing]
 [Just 5, Just 4, Nothing, Nothing]
 [Just 5, Just 6, Just 7, Just 8]
 ]
-}

disambiguateBoard :: Int -> [[a]] -> [[Maybe a]]
disambiguateBoard n = myMap1 (disambiguateList n)

 
myMap2 :: (a -> b) -> [a] -> [b]
myMap2 f = foldr (\x y -> (f x):y) []

listOfHeads :: [[a]] -> [a]
listOfHeads = myMap2 head

listOfTails :: [[a]] -> [[a]]
listOfTails = myMap2 tail

{-
returns a list of (column,row) pairs that go down the first row and then across the bottom row.
eg. in a board with 3 rows and 3 columns
3
2
1
  1  2  3
it returns [(1,3),(1,2),(1,1),(2,1),(3,1)]
-}
makeL :: BoardState -> [(Int,Int)]
makeL st = let
              nr = numRows st
              nc = numColumns st
              x1 = take nr (repeat 1)
              x2 = if nc == 1 then [] else [2..nc]
              y1 = myReverse [1 .. nr]
              y2 = take (nc-1) (repeat 1)
              x = x1 ++ x2
              y = y1 ++ y2
           in
              myZip x y

--same as makeL but for diagonals backwards
makeLRefl :: BoardState -> [(Int,Int)]
makeLRefl st = let
              nr = numRows st
              nc = numColumns st
              x1 = take nr (repeat 1)
              x2 = if nc == 1 then [] else [2..nc]
              y1 = [1 .. nr]
              y2 = take (nc-1) (repeat 6)
              x = x1 ++ x2
              y = y1 ++ y2
           in
              myZip x y



-- myZip [a1, ... , an] [b1, ... , bn] = [(a1,b1), .. , (an,bn)]
myZip :: [a] -> [b] -> [(a,b)]
myZip = zipWith (curry id)


-- returns the forward diagonal in BoardState st which starts at col c and row r. This should always be called with xs=[]
oneDiagForward :: (BoardState , (Int,Int), [Maybe Piece]) -> [Maybe Piece]
oneDiagForward (st, (c,r), xs) = case nextPieceDiagForward (st, (c-1,r-1)) of
                     Just piece -> oneDiagForward (st, (c+1,r+1), (piece): xs)
                     Nothing -> myReverse xs

-- returns the backward diagonal in BoardState st which starts at col c and row r. This should always be called with xs=[]
oneDiagBackward :: (BoardState , (Int,Int), [Maybe Piece]) -> [Maybe Piece]
oneDiagBackward (st, (c,r), xs) = case nextPieceDiagBackward (st, (c-1,r+1)) of
                     Just piece -> oneDiagBackward (st, (c+1,r-1), (piece): xs)
                     Nothing -> myReverse xs

nextPieceDiagForward :: (BoardState , (Int,Int)) -> Maybe (Maybe Piece)
nextPieceDiagForward (st, (c,r)) = case oneUpOneRight(st, (c,r)) of 
                    Just (col, row) -> Just (elemAtColRow (st, (col,row)))
                    Nothing -> Nothing 


nextPieceDiagBackward :: (BoardState , (Int,Int)) -> Maybe (Maybe Piece)
nextPieceDiagBackward (st, (c,r)) = case oneDownOneRight(st, (c,r)) of 
                    Just (col, row) -> Just (elemAtColRow (st, (col,row)))
                    Nothing -> Nothing 

--keeps giving (column, row) integer pairs that move one row up and one column right. When int goes out of the board, it returns nothing
oneUpOneRight :: (BoardState , (Int,Int)) -> Maybe (Int,Int)
oneUpOneRight (st, (c,r)) = if ((c >= (numColumns st)) || (r >= (numRows st))) then Nothing else (Just (c+1,r+1))

--keeps giving (column, row) integer pairs that move one row down and one column right. When int goes out of the board, it returns nothing
oneDownOneRight :: (BoardState , (Int,Int)) -> Maybe (Int,Int)
oneDownOneRight (st, (c,r)) = if ((c >= (numColumns st)) || (r <= 1)) then Nothing else (Just (c+1,r-1))

elemAtTwoD :: (Int,Int) -> [[a]] -> a
elemAtTwoD (col,row) ary = elemAt row (elemAt col ary)


-- returns the element in column col and row row in the disambiguated boardState st (column and row numbering starts at 1)
elemAtColRow :: (BoardState, (Int,Int)) -> Maybe Piece
elemAtColRow (st, (col, row)) = elemAtTwoD ((col-1),(row-1)) (columns st)

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f n [] = n
myfoldr f n (x:l) = f x (foldr f n l)


-- this function takes in a piece (Yellow or Red) and an array [Maybe Piece].
-- It returns true if array contains n consecutive pieces of color. Otherwise it returns false.
-- the count must be initially 0
-- winFlag must be initially false
arrayWinChecker :: (Piece, Int, Int, [Maybe Piece], Bool) -> Bool
arrayWinChecker (color, n, count, list , winFlag) = myCheckWinHelper (color, n, count, (list ++ [Nothing]), winFlag)

-- this function takes in a piece (Yellow or Red) and an array [Maybe Piece].
-- It returns true if array contains n consecutive pieces of color. Otherwise it returns false.
-- the count must be initially 0
-- winFlag must be initially false
myCheckWinHelper :: (Piece, Int, Int, [Maybe Piece], Bool) -> Bool
myCheckWinHelper (color, n, count, [], winFlag) = winFlag -- since the array is empty, we are done reading the array
myCheckWinHelper (color, n, count, x:xs, winFlag) = if (count == n) --player won
                                                  then True
                                                  else( -- in the reading we have done so far, no player has won
                                                     if (x == (Just color)) -- read a piece of the color we are interested in
                                                     then (myCheckWinHelper (color, n, count+1, xs, False))
                                                     else (myCheckWinHelper (color, n, 0, xs, False)))


-- this function takes a piece (Yellow or red), a number n (number of consecutive pieces neded to win),
-- a 2d array [[Maybe Piece]], and a winFlag which must initially be false
-- twoDarrayWinChecker returns true if in any of the subarrays, there are n concecutive piece. False otherwise
twoDarrayWinChecker :: (Piece, Int, [[Maybe Piece]], Bool) -> Bool
twoDarrayWinChecker (color, n, ary, True) = True -- if any subarray found a win, return win
twoDarrayWinChecker (color, n, [], winFlag) = False -- if the array is empty, we have looked through all the subarrays and found no wins. return false
twoDarrayWinChecker (color, n, x:xs, False) = twoDarrayWinChecker (color, n, xs, arrayWinChecker (color, n, 0, x, False))


