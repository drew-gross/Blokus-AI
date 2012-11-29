module Move(
	Move(Move, piece, position),
	apply,
	getMove,
	isValid, 

	validMovesForPlayer,

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
rubikDistanceToCenter move@(Move piece board position) _ = fromIntegral $ foldr (min) 100 rubikDistances --100 chosen arbitrarily, its larger than any board out there
	where
		centerInter = centerIntersection $ Board.grid board
		points = filledPointsOnBoard move
		rubikDistances :: [Int]
		rubikDistances = (flip rubikDistanceToIntersection) centerInter <$> points

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

apply :: Move -> Board
apply move@(Move piece board position) = modifiedBoard 
	where
		modifiedBoard = foldl (changeColorAt' $ Piece.color piece) board $ filledPointsOnBoard move
		changeColorAt' :: Color -> Board -> Point -> Board
		changeColorAt' color board = changeColorAt board color

filledPointsOnBoard :: Move -> [Point]
filledPointsOnBoard (Move piece _ position) = plus position <$> filledPoints piece

validMovesForPlayer :: Player -> Board -> [Move]
validMovesForPlayer (Player pieces _ _ _) board = concat $ validMovesForPiece board <$> pieces

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