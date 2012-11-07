import Debug.Trace

import Color
import Point
import Grid
import Display
import Player
import Board
import Piece
import Move

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
read1IndexdPoint = (flip minus $ Point 1 1)
getPieceFromIndex (Player pieces _) index = pieces !! index
moveFromUserInput (Player pieces _) pieceIndex rotation point = Move (rotations (pieces !! pieceIndex) !! rotation) point

getMove :: Board -> Player -> IO (Move, Board, Player)
getMove board player = do
	pieceIndex <- fmap read1IndexdIndex $ prompt $ displayToUserForPlayer board player ++ "\n" ++ (display player) ++ "\n" ++ "Enter piece number: "
	rotationNumber <- fmap read1IndexdIndex $ prompt $ "Enter rotation number:\n" ++ (displayNumberedList $ rotations $ getPieceFromIndex player pieceIndex)
	putStr $ displayToUserForPlayer board player
	point <- fmap read1IndexdPoint getPoint
	return (moveFromUserInput player pieceIndex rotationNumber point, 
		addPiece board (moveFromUserInput player pieceIndex rotationNumber point), 
		removePiece player $ (pieces player !! pieceIndex))

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
	updatedBoard = addPiece board move
	updatedPlayer = removePiece player (piece move)
	in (updatedBoard, updatedPlayer)

playGame :: (Board, [Player]) -> IO Board
playGame (board, nextHuman:nextAI:otherPlayers) = do
	(nextBoard, nextPlayer) <- completeUserTurn (board, nextHuman)
	let
		(nextBoard2, nextPlayer2) = completeAiTurn (nextBoard, nextAI)
	playGame (nextBoard2, (otherPlayers) ++ [nextPlayer] ++ [nextPlayer2])

main = playGame (empty2PlayerBoard, [newPlayer Red, newPlayer Blue])