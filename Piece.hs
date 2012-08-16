module Piece(
	Piece(Piece, grid),
	rotations,
) where

import Grid
import Color
import Utilities

data Piece = Piece {grid :: Grid Color} deriving (Show)

instance ShowToUser Piece where
	showToUser =  showToUser . grid

rotations :: Piece -> [Piece]
rotations (Piece grid) = [Piece grid] ++ [Piece $ rotate90 grid] ++ [Piece $ rotate180 grid] ++ [Piece $ rotate270 grid]