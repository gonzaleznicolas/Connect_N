module Assignment3IO where 

import GameLogic
import Data.List (intersperse)

main :: IO ()
main = do
    putStrLn "enter number of rows"
    rowsString <- getLine
    let rows = read rowsString :: Int 
    putStrLn "enter number of columns"
    columnsString <- getLine 
    let columns = read columnsString :: Int
    putStrLn "enter number to connect"
    nString <- getLine 
    let n = read nString :: Int 
    putStrLn "Yellow moves first"
    playGame (initializeBoard rows columns n Red)

initializeBoard :: Int -> Int -> Int -> Piece -> BoardState 
initializeBoard rows cols numConn lastPlayer = 
    BS {
        theBoard = take cols (repeat []),
        numRows = rows,
        numColumns = cols,
        numToConnect = numConn,
        lastMove = lastPlayer
    }

playGame :: BoardState -> IO ()
playGame bs =
    case checkBoardFull bs of
        True -> putStrLn "game is a draw"
        False -> playGameAvail bs

{-
This is too close to one of the functions you must implement, so
I left it undefined, for now.

This checks to see if all columns are full.

checkBoardFull :: BoardState -> Bool
checkBoardFull bs = 
    let 
        cols = theBoard bs
    in
        all (\col -> length col == numRows bs) cols 
-}


myFunction bs moved1 moved2 = if (lastMove bs == Yellow) then moved1 else moved2

playGameAvail :: BoardState -> IO ()
playGameAvail bs = do
    putStrLn $ "enter column number of next move: 1 - " ++ show (numColumns bs)

    nextMove1 <- getLine
    --let nextMove2 = unJust (suggestMove bs)

    let moved1 = makeMove bs (read nextMove1 :: Int)
    --let moved2 = makeMove bs nextMove2

    let moved = moved1 --myFunction bs moved1 moved2

    case moved of 
        Nothing -> do
            putStrLn "invalid move, try again"
            playGameAvail bs 
        Just newBS -> do
            printBoard newBS 
            case checkWin newBS of 
                Nothing -> playGame newBS
                Just Red -> putStrLn "\nRed wins!!"
                Just Yellow -> putStrLn "\nYellow wins!!"

printBoard :: BoardState -> IO ()
printBoard bs = 
    let
        maybeRows = rows bs
        maybePieceToString :: Maybe Piece -> String 
        maybePieceToString p = 
            case p of
                Nothing ->     "        "
                Just Red ->    " Red    "
                Just Yellow -> " Yellow "
        rowsToStrings = map (map maybePieceToString) maybeRows 
        rowsVBars = map (intersperse "|" ) rowsToStrings
        rowsToLines = map (\r -> concat r ++ "\n") rowsVBars
        rowLength = length (head rowsToLines)
        longLine = (take rowLength (repeat '-')) ++ "\n" 
        rowsUnderlined = intersperse longLine rowsToLines
    in 
        putStrLn (concat rowsUnderlined)

