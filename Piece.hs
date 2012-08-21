module Piece(
	Piece(Piece, grid),
	rotations,
) where

import Data.List

import Grid
import Color
import Utilities

data Piece = Piece {grid :: Grid Color} deriving (Show, Eq)

instance ShowToUser Piece where
	showToUser =  showToUser . grid

rotations :: Piece -> [Piece]
rotations (Piece grid) = nub $ [Piece grid] ++ [Piece $ rotate90 grid] ++ [Piece $ rotate180 grid] ++ [Piece $ rotate270 grid] ++[Piece $ flipAboutVertical grid] ++ [Piece $ rotate90 $ flipAboutVertical grid] ++ [Piece $ rotate180 $ flipAboutVertical grid] ++ [Piece $ rotate270 $ flipAboutVertical grid]