module Move(
	Move(Move, piece, position),
	squaresUsed
) where

import Piece
import Point

data Move = Move {piece :: Piece, position :: Point}

squaresUsed (Move piece _) = filledPointsCount piece