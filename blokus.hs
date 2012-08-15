import Debug.Trace

import Color
import Point
import Grid
import Utilities
import Player

type Board = Grid Color
type Piece = Grid Color

prevPoint :: Piece -> Point -> Point
prevPoint piece (Point 0 y) = Point (width piece - 1) (y - 1)
prevPoint piece point = Point (x point - 1) (y point)

maxPoint :: Piece -> Point
maxPoint piece = Point ((width piece) - 1) ((height piece) - 1)


defaultSize = 14
newBoard = Grid (take (defaultSize * defaultSize) $ repeat Empty) defaultSize
newPlayer = (Player 
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

safeAccess :: [a] -> Int -> [a]
safeAccess list index = if index < 0 || index >= length list
					then []
					else [list !! index]

safeBoardAccess :: Board -> Point -> [Color]
safeBoardAccess board point
	| not $ containsPoint board point = []
	| otherwise = [itemAt board point]

corners :: Board -> Point -> [Color]
corners board point
	| not $ containsPoint board point = error "index out of range"
	| otherwise = (safeBoardAccess board (ulPoint point)) ++ (safeBoardAccess board (urPoint point)) ++ (safeBoardAccess board (drPoint point)) ++ (safeBoardAccess board (dlPoint point))

sides :: Board -> Point -> [Color]
sides board point
	| not $ containsPoint board point = error "index out of range"
	| otherwise = (safeBoardAccess board $ leftPoint point) ++ (safeBoardAccess board $ rightPoint point) ++ (safeBoardAccess board $ upPoint point) ++ (safeBoardAccess board $ downPoint point)

isOpenToColor :: Board -> Color -> Point -> Bool
isOpenToColor board color (Point 0 0) = itemAt board (Point 0 0) == Empty
isOpenToColor board color point = (color `elem` (corners board point)) && (not (color `elem` sides board point)) && ((itemAt board point) == Empty)

addPieceSquareToBoard :: Board -> Piece -> Point -> Point -> Board
addPieceSquareToBoard board piece boardLocation (Point 0 0) = changeItemAt board (itemAt piece (Point 0 0)) boardLocation
addPieceSquareToBoard board piece boardLocation pieceLocation = 
	let	nextPieceLocation = (prevPoint piece pieceLocation)
		color = (itemAt piece pieceLocation)
		updatedBoard = (changeItemAt board color (boardLocation `plus` pieceLocation))
	in addPieceSquareToBoard updatedBoard piece boardLocation nextPieceLocation

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

playGame :: (Board, Player) -> IO Board
playGame (board, player) = do
	(nextBoard, nextPlayer) <- completeUserTurn (board, player)
	putStr $ showToUser nextBoard
	playGame (nextBoard, nextPlayer)

main = playGame (newBoard, newPlayer)