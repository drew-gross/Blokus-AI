import Debug.Trace

import Color
import Point
import Grid
import Display
import Player
import Board
import Piece
import Move

prevPoint :: Piece -> Point -> Point
prevPoint (Piece grid _) (Point 0 y) = Point (width grid - 1) (y - 1)
prevPoint _ point = Point (x point - 1) (y point)

addPieceToBoardHelper :: Board -> Move -> Point -> Board
addPieceToBoardHelper board move (Point 0 0) = Board (changeItemAt (Board.grid board) (itemAt (Piece.grid $ piece move) origin) (position move)) (startPoints board)
addPieceToBoardHelper board move pieceLocation = 
	let	nextPieceLocation = prevPoint (piece move) pieceLocation
		color = itemAt (Piece.grid (piece move)) pieceLocation
		updatedGrid = changeItemAt (Board.grid board) color $ (position move) `plus` pieceLocation
	in addPieceToBoardHelper (Board updatedGrid (startPoints board)) move nextPieceLocation

addPieceToBoard :: Board -> Move -> Board
addPieceToBoard board move = addPieceToBoardHelper board move (maxPoint $ Piece.grid $ piece move)

isMoveInBounds :: Board -> Move -> Bool
isMoveInBounds board (Move piece position)
	| x position < 0 = False
	| y position < 0 = False
	| x (position `plus` maxPoint (Piece.grid piece)) >= width (Board.grid board) = False
	| y (position `plus` maxPoint (Piece.grid piece)) >= height (Board.grid board) = False
	| otherwise = True

isMoveValid :: Board -> Move -> Bool
isMoveValid board move
	| not $ isMoveInBounds board move = False
	| not $ and $ map (isPointAdjacentToColor board color) pointsOnBoard = False
	| any (isPointOpenToColor board color) pointsOnBoard = True
	| otherwise = False
	where
		color = Piece.color $ piece move
		pointsInPiece = filledPoints $ piece move
		pointsOnBoard = map (plus $ position move) pointsInPiece

allInvalidMovesForPieceRotation :: Board -> Piece -> [Move]
allInvalidMovesForPieceRotation board piece = let
		maxPlacementPoint = ((maxPoint $ Board.grid board) `minus` (maxPoint $ Piece.grid piece))
	in map (Move piece) (range origin maxPlacementPoint)

allValidMovesForPieceRotation :: Board -> Piece -> [Move]
allValidMovesForPieceRotation board piece = filter (isMoveValid board) (allInvalidMovesForPieceRotation board piece)

allValidMovesForPiece :: Board -> Piece -> [Move]
allValidMovesForPiece board piece = concatMap (allValidMovesForPieceRotation board) (rotations piece)

allValidMovesForPlayer :: Board -> Player -> [Move]
allValidMovesForPlayer board player = concatMap (allValidMovesForPiece board) (pieces player)

read1IndexdIndex = (flip (-) 1) . read
getPieceFromUserString player string = (pieces player) !! read1IndexdIndex string
moveFromUserInput player pieceIndexStr rotationNumberStr index0point = Move (rotations piece !! rotation) point
	where
		point = index0point `minus` (Point 1 1)
		piece = getPieceFromUserString player pieceIndexStr
		rotation = read1IndexdIndex rotationNumberStr

getMove :: Board -> Player -> IO (Move, Board, Player)
getMove board player = do
	pieceIndexStr     <- prompt $ displayToUserForPlayer board player ++ "\n" ++ (display player) ++ "\n" ++ "Enter piece number: "
	rotationNumberStr <- prompt $ "Enter rotation number:\n" ++ (displayNumberedList $ rotations $ getPieceFromUserString player pieceIndexStr)
	putStr $ displayToUserForPlayer board player
	index0point <- getPoint
	return (moveFromUserInput player pieceIndexStr rotationNumberStr index0point, 
		addPieceToBoard board (moveFromUserInput player pieceIndexStr rotationNumberStr index0point), 
		removePiece player $ (pieces player !! read1IndexdIndex pieceIndexStr))

completeUserTurn :: (Board, Player) -> IO (Board, Player)
completeUserTurn (board, player) = do
	(move, updatedBoard, updatedPlayer) <- getMove board player
	if isMoveValid board move then do
		continue <- prompt $ displayToUserForPlayer updatedBoard updatedPlayer ++ "\n" ++ "Is this correct? (y/n): "
		if continue == "y" then
			return (updatedBoard, updatedPlayer)
		else
			completeUserTurn (board, player)
	else do
		putStr "Invalid Move!\n"
		completeUserTurn (board, player)
		
completeAiTurn :: (Board, Player) -> (Board, Player)
completeAiTurn (board, player) = let
	move = head $ allValidMovesForPlayer board player
	updatedBoard = addPieceToBoard board move
	updatedPlayer = removePiece player (piece move)
	in (updatedBoard, updatedPlayer)

playGame :: (Board, [Player]) -> IO Board
playGame (board, nextHuman:nextAI:otherPlayers) = do
	(nextBoard, nextPlayer) <- completeUserTurn (board, nextHuman)
	let
		(nextBoard2, nextPlayer2) = completeAiTurn (nextBoard, nextAI)
	playGame (nextBoard2, (otherPlayers) ++ [nextPlayer] ++ [nextPlayer2])

main = playGame (empty2PlayerBoard, [newPlayer Red, newPlayer Blue])