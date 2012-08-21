import Debug.Trace

import Color
import Point
import Grid
import Display
import Player
import Board
import Piece

prevPoint :: Piece -> Point -> Point
prevPoint (Piece grid) (Point 0 y) = Point (width grid - 1) (y - 1)
prevPoint _ point = Point (x point - 1) (y point)

defaultSize = 14
defaultStartPoints = [Point 4 4, Point 9 9]
newBoard = Board (Grid (take (defaultSize * defaultSize) $ repeat Empty) defaultSize) defaultStartPoints
newRedPlayer = (Player 
			(
			 [
			 Piece $ Grid [Red, Red, Red, Red, Red, Empty] 3,
			 Piece $ Grid [Red] 1, 
			 Piece $ Grid [Red, Empty, Red, Red] 2,
			 Piece $ Grid [Red, Red] 1,
			 Piece $ Grid [Red, Red, Red, Red] 2,
			 Piece $ Grid [Red, Red, Red] 1
			 ]
			)
			 Red)
newBluePlayer = (Player 
			(
			 [
			 Piece $ Grid [Blue, Blue, Blue, Blue, Blue, Empty] 3,
			 Piece $ Grid [Blue] 1, 
			 Piece $ Grid [Blue, Empty, Blue, Blue] 2,
			 Piece $ Grid [Blue, Blue] 1,
			 Piece $ Grid [Blue, Blue, Blue, Blue] 2,
			 Piece $ Grid [Blue, Blue, Blue] 1
			 ]
			)
			 Blue)

addPieceToBoardHelper :: Board -> Piece -> Point -> Point -> Board
addPieceToBoardHelper board piece boardLocation (Point 0 0) = Board (changeItemAt (Board.grid board) (itemAt (Piece.grid piece) (Point 0 0)) boardLocation) (startPoints board)
addPieceToBoardHelper board piece boardLocation pieceLocation = 
	let	nextPieceLocation = (prevPoint piece pieceLocation)
		color = (itemAt (Piece.grid piece) pieceLocation)
		updatedGrid = (changeItemAt (Board.grid board) color (boardLocation `plus` pieceLocation))
	in addPieceToBoardHelper (Board updatedGrid (startPoints board)) piece boardLocation nextPieceLocation

addPieceToBoard :: Board -> Piece -> Point -> Board
addPieceToBoard board piece boardPoint = addPieceToBoardHelper board piece boardPoint (maxPoint $ Piece.grid piece)

isPieceInBounds :: Board -> Piece -> Point -> Bool
isPieceInBounds board piece point 
	| x point < 0 = False
	| y point < 0 = False
	| x (point `plus` maxPoint (Piece.grid piece)) >= width (Board.grid board) = False
	| y (point `plus` maxPoint (Piece.grid piece)) >= height (Board.grid board) = False
	| otherwise = True

isMoveValid :: Board -> Piece -> Point -> Bool
isMoveValid board piece point
	| not $ isPieceInBounds board piece point = trace "piece out of bounds" False
	| not $ and $ map (isPointValidToColor board (Piece.color piece)) (filledPoints piece) = trace "point invalid to color" False
	| or $ map (isPointOpenToColor board (Piece.color piece)) (map (plus point) (filledPoints piece)) = True
	| otherwise = traceShow ("no points open to color", (filledPoints piece)) False

completeUserTurn :: (Board, Player) -> IO (Board, Player)
completeUserTurn (board, player) = do
	printToUserForPlayer board player
	printDisplay player
	putStr "Enter piece number: "
	pieceIndexStr <- getLine
	let 
		pieceIndex = read pieceIndexStr - 1
		piece = pieces player !! pieceIndex
	putStr "Enter rotation number:\n"
	printDisplayNumberedList $ rotations piece
	rotationNumberStr <- getLine
	let
		rotationNumber = read rotationNumberStr - 1
		rotatedPiece = rotations piece !! rotationNumber
	printToUserForPlayer board player
	putStr "Enter x: "
	xStr <- getLine
	putStr "Enter y: "
	yStr <- getLine
	let
		x = (read xStr) - 1
		y = (read yStr) - 1
		point = Point x y
		validMove = isMoveValid board rotatedPiece point
		updatedBoard = addPieceToBoard board rotatedPiece point
		updatedPlayer = removePiece player pieceIndex
	if validMove then do
		printToUserForPlayer updatedBoard updatedPlayer
		putStr "Is this correct? (y/n): "
		continue <- getLine
		if continue == "y" then
			return (updatedBoard, updatedPlayer)
		else
			completeUserTurn (board, player)
	else do
		putStr "Invalid Move!\n"
		completeUserTurn (board, player)

playGame :: (Board, [Player]) -> IO Board
playGame (board, players) = do
	(nextBoard, nextPlayer) <- completeUserTurn (board, head players)
	playGame (nextBoard, (tail players) ++ [nextPlayer])

main = playGame (newBoard, [newRedPlayer, newBluePlayer])