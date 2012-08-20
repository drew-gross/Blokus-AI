import Debug.Trace

import Color
import Point
import Grid
import Utilities
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

addPieceSquareToBoard :: Board -> Piece -> Point -> Point -> Board
addPieceSquareToBoard board piece boardLocation (Point 0 0) = Board (changeItemAt (Board.grid board) (itemAt (Piece.grid piece) (Point 0 0)) boardLocation) (startPoints board)
addPieceSquareToBoard board piece boardLocation pieceLocation = 
	let	nextPieceLocation = (prevPoint piece pieceLocation)
		color = (itemAt (Piece.grid piece) pieceLocation)
		updatedGrid = (changeItemAt (Board.grid board) color (boardLocation `plus` pieceLocation))
	in addPieceSquareToBoard (Board updatedGrid (startPoints board)) piece boardLocation nextPieceLocation

addPieceToBoard :: Board -> Piece -> Point -> Board
addPieceToBoard board piece boardPoint = addPieceSquareToBoard board piece boardPoint (maxPoint $ Piece.grid piece)

completeUserTurn :: (Board, Player) -> IO (Board, Player)
completeUserTurn (board, player) = do
	printToUser player
	putStr "Enter piece number: "
	pieceIndexStr <- getLine
	let 
		pieceIndex = read pieceIndexStr - 1
		piece = pieces player !! pieceIndex
	putStr "Enter rotation number:\n"
	printNumberedListToUser $ rotations piece
	rotationNumberStr <- getLine
	let
		rotationNumber = read rotationNumberStr - 1
		rotatedPiece = rotations piece !! rotationNumber
	putStr "Enter x: "
	xStr <- getLine
	putStr "Enter y: "
	yStr <- getLine
	let
		point = Point (read xStr) (read yStr)
		updatedBoard = addPieceToBoard board rotatedPiece point
		updatedPlayer = removePiece player pieceIndex
	putStr $ displayForPlayer updatedBoard updatedPlayer
	putStr "Is this correct? (y/n): "
	continue <- getLine
	if continue == "y" || continue == "Y" then
		return (updatedBoard, updatedPlayer)
	else
		completeUserTurn (board, player)

playGame :: (Board, [Player]) -> IO Board
playGame (board, players) = do
	putStr $ displayForPlayer board (head players)
	(nextBoard, nextPlayer) <- completeUserTurn (board, head players)
	playGame (nextBoard, (tail players) ++ [nextPlayer])

main = playGame (newBoard, [newRedPlayer, newBluePlayer])