module Move(
	Move(Move, piece, position),
	newHuman,
	newComputer,

	squaresUsed,
	launchPointsGained,
	enemyLaunchPointsLost,
	rubikDistanceToCenter
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

newComputer :: (Fractional a, Ord a) => Color -> [(a, Move -> Player -> a)]-> Player
newComputer color chromosome = Player (startingPieces color) color $ completeAiTurn chromosome

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

rubikDistanceToCenter :: Fractional a => Move -> Player -> a
rubikDistanceToCenter move@(Move piece board position) enemy = fromIntegral $ foldr (min) 100 rubikDistances --100 chosen arbitrarily, its larger than any board out there
	where
		centerInter = centerIntersection $ Board.grid board
		points = filledPointsOnBoard move
		rubikDistances :: [Int]
		rubikDistances = (flip rubikDistanceToIntersection) centerInter <$> points

fitness :: Fractional a => Move -> [(a, Move -> Player -> a)] -> Player -> a
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
validMovesForPieceRotation board = (filter isValid) . (candidateMovesForPieceRotation board)

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
apply move@(Move piece board position) = modifiedBoard 
	where
		modifiedBoard = foldl (changeColorAt' $ Piece.color piece) board $ filledPointsOnBoard move
		changeColorAt' :: Color -> Board -> Point -> Board
		changeColorAt' color board = changeColorAt board color

filledPointsOnBoard :: Move -> [Point]
filledPointsOnBoard (Move piece _ position) = plus position <$> filledPoints piece

validMovesForPlayer :: Player -> Board -> [Move]
validMovesForPlayer (Player pieces _ _) board = concat $ validMovesForPiece board <$> pieces

aiSelectedMove :: (Ord a, Fractional a) => [(a, Move -> Player -> a)] -> Player -> Board -> Player -> Maybe Move
aiSelectedMove chromosome player board enemy = maybeHead $ reverse $ sortBy (compare `on` fitness' enemy chromosome) $ validMovesForPlayer player board
	where
		fitness' :: Fractional a => Player -> [(a, Move -> Player -> a)] -> Move -> a
		fitness' player chromosome move = fitness move chromosome player

completeAiTurn :: (Ord a, Fractional a) => [(a, Move -> Player -> a)] -> Player -> Board -> Player -> IO (Maybe (Board, Player))
completeAiTurn chromosome player board enemy = return $ (,) <$> updatedBoard <*> updatedPlayer
	where
		move = aiSelectedMove chromosome player board enemy
		updatedBoard = apply <$> move
		updatedPlayer = removePiece player <$> piece <$> move

isInBounds :: Move -> Bool
isInBounds (Move (Piece grid _) board position)
	| not $ containsPoint (Board.grid board) position = False
	| not $ containsPoint (Board.grid board) $ position `plus` (maxPoint grid) = False
	| otherwise = True

isValid :: Move -> Bool
isValid move@(Move piece board position)
	| not $ isInBounds move = False --move is outside of board
	| any (\point -> unsafeColorAt board point /= Empty) pointsOnBoard = False --move is overlapping another piece
	| any (isPointAdjacentToColor board color) pointsOnBoard = False --side of piece is touching its own color
	| any (isPointLaunchPointForColor board color) pointsOnBoard = True
	| otherwise = False
	where
		color = Piece.color piece
		pointsOnBoard = plus position <$> filledPoints piece