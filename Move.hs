module Move(
	Move(Move, piece, position),
	squaresUsed,
	apply,
	isValid,
	fitness,
	validMovesForPiece,
	validMovesForPlayer,
	newHuman,
	newComputer
) where

import Control.Applicative

import Data.Maybe
import Data.Function
import Data.List

import Utilities
import Player
import Piece
import Point
import Board
import Grid
import Color

data Move = Move {piece :: Piece, board :: Board, position :: Point}

newHuman :: Color -> Player
newHuman color = Player (startingPieces color) color completeUserTurn

newComputer :: Color -> Player
newComputer color = Player (startingPieces color) color completeAiTurn

coefficient1 :: Fractional a => a
coefficient1 = 1.0

coefficient2 :: Fractional a => a
coefficient2 = 1.0

squaresUsed :: Fractional a => Move -> a
squaresUsed (Move piece _ _) = coefficient1 * (fromIntegral $ filledPointsCount piece)

launchPointsGained :: Fractional a => Move -> a
launchPointsGained move@(Move piece board _) = coefficient2 * (fromIntegral ((numOfLaunchPointsForColor (apply move) color) - (numOfLaunchPointsForColor board color)))
	where
		color = Piece.color piece

fitness :: Fractional a => Move -> a
fitness move = squaresUsed move + launchPointsGained move

candidateMovesForPieceRotation :: Board -> Piece -> [Move]
candidateMovesForPieceRotation board@(Board boardGrid _) piece@(Piece pieceGrid identifier) = let
		maxPlacementPoint = ((maxPoint boardGrid) `minus` (maxPoint pieceGrid))
	in map (Move piece board) $ range origin maxPlacementPoint

validMovesForPieceRotation :: Board -> Piece -> [Move]
validMovesForPieceRotation board piece = filter isValid $ candidateMovesForPieceRotation board piece

validMovesForPiece :: Board -> Piece -> [Move]
validMovesForPiece board piece = concatMap (validMovesForPieceRotation board) $ rotations piece

getMove :: Player -> Board -> IO (Maybe (Move, Board, Player))
getMove player board = do
	piece <- getRotatedPiece player board
	move <- Move <$> (return piece) <*> (return board) <*> (fmap read1IndexedPoint getPoint)
	return $ Just (move, apply move, removePiece player piece)

completeUserTurn :: Player -> Board -> IO (Maybe (Board, Player))
completeUserTurn player board = do
	m <- getMove player board
	if isNothing m then
		return Nothing
	else do
		let (move, updatedBoard, updatedPlayer) = fromJust m
		if isValid move then do
			continue <- prompt $ displayToUserForPlayer updatedPlayer updatedBoard ++ "\n" ++ "Is this correct? (y/n): "
			if continue == "y" then
				return $ Just (updatedBoard, updatedPlayer)
			else
				completeUserTurn player board
		else do
			putStr "Invalid Move!\n"
			completeUserTurn player board

apply :: Move -> Board
apply (Move piece board position) = modifiedBoard 
	where
		modifiedBoard = foldl (changeColorAt' $ Piece.color piece) board pointsOnBoard
		pointsOnBoard = map (plus position) $ filledPoints piece
		changeColorAt' :: Color -> Board -> Point -> Board
		changeColorAt' color board = changeColorAt board color

validMovesForPlayer :: Player -> Board -> [Move]
validMovesForPlayer (Player pieces _ _) board = concatMap (validMovesForPiece board) pieces

aiSelectedMove :: Player -> Board -> Maybe Move
aiSelectedMove player board = maybeHead $ reverse $ sortBy (compare `on` fitness) $ validMovesForPlayer player board

completeAiTurn :: Player -> Board -> IO (Maybe (Board, Player))
completeAiTurn player board = return $ (,) <$> updatedBoard <*> updatedPlayer
	where
		move = aiSelectedMove player board
		updatedBoard = fmap apply move
		updatedPlayer = fmap (removePiece player) $ fmap piece move

isInBounds :: Move -> Bool
isInBounds (Move (Piece grid _) board position)
	| not $ isPointInBounds board position = False
	| not $ isPointInBounds board $ position `plus` (maxPoint grid) = False
	| otherwise = True

isValid :: Move -> Bool
isValid move@(Move piece board position)
	| not $ isInBounds move = False --move is outside of board
	| any (\point -> colorAt board point /= Empty) pointsOnBoard = False --move is overlapping another piece
	| any (isPointAdjacentToColor board color) pointsOnBoard = False --side of piece is touching its own color
	| any (isPointLaunchPointForColor board color) pointsOnBoard = True
	| otherwise = False
	where
		color = Piece.color piece
		pointsOnBoard = map (plus position) $ filledPoints piece