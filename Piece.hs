module Piece(
	Piece(Piece, grid)
) where

import Grid
import Color
import Utilities

data Piece = Piece {grid :: Grid Color} deriving (Show)

instance ShowToUser Piece where
	showToUser =  showToUser . grid

rotations :: Piece -> [Piece]
rotations (Piece grid) = [(Piece grid)]