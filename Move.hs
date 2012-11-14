module Move(
	Move(Move, piece, position),
	squaresUsed
) where

import Piece
import Point

data Move = Move {piece :: Piece, position :: Point}

coefficient :: Fractional a => a
coefficient = 1.0

squaresUsed :: Fractional a => Move -> a
squaresUsed (Move piece _) = coefficient * (fromIntegral $ filledPointsCount piece)