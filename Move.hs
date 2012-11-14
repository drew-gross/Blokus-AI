module Move(
	Move(Move, piece, position),
	squaresUsed,
	apply,
	isValid,
	validMovesForPiece
) where

import Piece
import Point
import Board
import Grid
import Color

data Move = Move {piece :: Piece, board :: Board, position :: Point}

coefficient :: Fractional a => a
coefficient = 1.0

squaresUsed :: Fractional a => Move -> a
squaresUsed (Move piece _ _) = coefficient * (fromIntegral $ filledPointsCount piece)

candidateMovesForPieceRotation :: Board -> Piece -> [Move]
candidateMovesForPieceRotation board@(Board boardGrid _) piece@(Piece pieceGrid identifier) = let
		maxPlacementPoint = ((maxPoint boardGrid) `minus` (maxPoint pieceGrid))
	in map (Move piece board) $ range origin maxPlacementPoint

validMovesForPieceRotation :: Board -> Piece -> [Move]
validMovesForPieceRotation board piece = filter isValid (candidateMovesForPieceRotation board piece)

validMovesForPiece :: Board -> Piece -> [Move]
validMovesForPiece board piece = concatMap (validMovesForPieceRotation board) (rotations piece)

apply :: Move -> Board
apply (Move piece board position) = foldl (changeColorAt' $ Piece.color piece) board pointsOnBoard
	where
		pointsOnBoard = map (plus position) $ filledPoints piece
		changeColorAt' :: Color -> Board -> Point -> Board
		changeColorAt' color board = changeColorAt board color

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
		pointsInPiece = filledPoints piece
		pointsOnBoard = map (plus position) pointsInPiece