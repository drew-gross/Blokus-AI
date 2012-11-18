module Move(
	Move(Move, piece, position),
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

coefficient3 :: Fractional a => a
coefficient3 = 1.0

squaresUsed :: Fractional a => Move -> Player -> a
squaresUsed (Move piece _ _) _ = fromIntegral $ filledPointsCount piece

launchPointsGained :: Fractional a => Move -> Player -> a
launchPointsGained move@(Move piece board _) _ = fromIntegral $ numOfLaunchPointsForColor (apply move) color - numOfLaunchPointsForColor board color
	where
		color = Piece.color piece

enemyLaunchPointsLost :: Fractional a => Move -> Player -> a
enemyLaunchPointsLost move@(Move piece board _) enemy = fromIntegral $ numOfLaunchPointsForColor board enemyColor - numOfLaunchPointsForColor (apply move) enemyColor
	where
		enemyColor = Player.color enemy

fitness :: Fractional a => Move -> [(a, Move -> Player -> a)]-> Player -> a
fitness move chromosome enemy = sum weightedValues
	where
		(coefficients, functions) = unzip chromosome
		values = ($ enemy) <$> ($ move) <$> functions
		weightedValues = zipWith (*) coefficients values

candidateMovesForPieceRotation :: Board -> Piece -> [Move]
candidateMovesForPieceRotation board@(Board boardGrid _) piece@(Piece pieceGrid identifier) = let
		maxPlacementPoint = ((maxPoint boardGrid) `minus` (maxPoint pieceGrid))
	in Move piece board <$> range origin maxPlacementPoint

validMovesForPieceRotation :: Board -> Piece -> [Move]
validMovesForPieceRotation board piece = filter isValid $ candidateMovesForPieceRotation board piece

validMovesForPiece :: Board -> Piece -> [Move]
validMovesForPiece board piece = concat $ validMovesForPieceRotation board <$> rotations piece

getMove :: Player -> Board -> Player -> IO (Maybe (Move, Board, Player))
getMove player board _ = do
	piece <- getRotatedPiece player board
	move <- Move <$> (return piece) <*> (return board) <*> read1IndexedPoint <$> getPoint
	return $ Just (move, apply move, removePiece player piece)

completeUserTurn :: Player -> Board -> Player -> IO (Maybe (Board, Player))
completeUserTurn player board enemy = do
	m <- getMove player board enemy
	if isNothing m then
		return Nothing
	else do
		let (move, updatedBoard, updatedPlayer) = fromJust m
		if isValid move then do
			continue <- prompt $ displayToUserForPlayer updatedPlayer updatedBoard ++ "\n" ++ "Is this correct? (y/n): "
			if continue == "y" then
				return $ Just (updatedBoard, updatedPlayer)
			else
				completeUserTurn player board enemy
		else do
			putStr "Invalid Move!\n"
			completeUserTurn player board enemy

apply :: Move -> Board
apply (Move piece board position) = modifiedBoard 
	where
		modifiedBoard = foldl (changeColorAt' $ Piece.color piece) board pointsOnBoard
		pointsOnBoard = plus position <$> filledPoints piece
		changeColorAt' :: Color -> Board -> Point -> Board
		changeColorAt' color board = changeColorAt board color

validMovesForPlayer :: Player -> Board -> [Move]
validMovesForPlayer (Player pieces _ _) board = concat $ validMovesForPiece board <$> pieces

chromosome = [(1.0, squaresUsed),(1.0, launchPointsGained),(1.0, enemyLaunchPointsLost)]

aiSelectedMove :: Player -> Board -> Player -> Maybe Move
aiSelectedMove player board enemy = maybeHead $ reverse $ sortBy (compare `on` fitness' enemy chromosome) $ validMovesForPlayer player board
	where
		fitness' :: Fractional a => Player -> [(a, Move -> Player -> a)] -> Move -> a
		fitness' player chromosome move = fitness move chromosome player

completeAiTurn :: Player -> Board -> Player -> IO (Maybe (Board, Player))
completeAiTurn player board enemy = return $ (,) <$> updatedBoard <*> updatedPlayer
	where
		move = aiSelectedMove player board enemy
		updatedBoard :: Maybe Board
		updatedBoard = apply <$> move
		updatedPlayer :: Maybe Player
		updatedPlayer = removePiece player <$> piece <$> move

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
		pointsOnBoard = plus position <$> filledPoints piece