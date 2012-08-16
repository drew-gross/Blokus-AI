import Debug.Trace

import Color
import Point
import Grid
import Utilities
import Player
import Board

type Piece = Grid Color

prevPoint :: Piece -> Point -> Point
prevPoint piece (Point 0 y) = Point (width piece - 1) (y - 1)
prevPoint piece point = Point (x point - 1) (y point)

defaultSize = 14
defaultStartPoints = [Point 4 4, Point 9 9]
newBoard = Board (Grid (take (defaultSize * defaultSize) $ repeat Empty) defaultSize) defaultStartPoints
newRedPlayer = (Player 
			(
			 [
			 Grid [Red] 1, 
			 Grid [Red, Empty, Red, Red] 2,
			 Grid [Red, Red] 1,
			 Grid [Red, Red, Red, Red] 2,
			 Grid [Red, Red, Red] 1
			 ]
			)
			 Red)
newBluePlayer = (Player 
			(
			 [
			 Grid [Blue] 1, 
			 Grid [Blue, Empty, Blue, Blue] 2,
			 Grid [Blue, Blue] 1,
			 Grid [Blue, Blue, Blue, Blue] 2,
			 Grid [Blue, Blue, Blue] 1
			 ]
			)
			 Blue)

addPieceSquareToBoard :: Board -> Piece -> Point -> Point -> Board
addPieceSquareToBoard board piece boardLocation (Point 0 0) = Board (changeItemAt (grid board) (itemAt piece (Point 0 0)) boardLocation) (startPoints board)
addPieceSquareToBoard board piece boardLocation pieceLocation = 
	let	nextPieceLocation = (prevPoint piece pieceLocation)
		color = (itemAt piece pieceLocation)
		updatedGrid = (changeItemAt (grid board) color (boardLocation `plus` pieceLocation))
	in addPieceSquareToBoard (Board updatedGrid (startPoints board)) piece boardLocation nextPieceLocation

addPieceToBoard :: Board -> Piece -> Point -> Int -> Board
addPieceToBoard board piece boardPoint 0 = addPieceSquareToBoard board piece boardPoint (maxPoint piece)
addPieceToBoard _ _ _ rotation = error "not implemented yet"

completeUserTurn :: (Board, Player) -> IO (Board, Player)
completeUserTurn (board, player) = do
	printToUser player
	putStr "Enter piece number: "
	pieceIndexStr <- getLine
	putStr "Enter x: "
	x <- getLine
	putStr "Enter y: "
	y <- getLine
	let
		point = Point (read x) (read y)
		pieceIndex = (read pieceIndexStr) - 1
		piece = ((pieces player) !! pieceIndex)
		updatedBoard = addPieceToBoard board piece point 0
		updatedPlayer = removePiece player pieceIndex
	return (updatedBoard, updatedPlayer)

playGame :: (Board, [Player]) -> IO Board
playGame (board, players) = do
	putStr $ displayForPlayer board (head players)
	(nextBoard, nextPlayer) <- completeUserTurn (board, head players)
	playGame (nextBoard, (tail players) ++ [nextPlayer])

main = playGame (newBoard, [newRedPlayer, newBluePlayer])