module Piece(
	Piece(Piece, grid),
	rotations,
	color,
	filledPoints,
	Piece.allPoints
) where

import Data.List

import Point
import Grid
import Color
import Utilities

data Piece = Piece {grid :: Grid Color} deriving (Show, Eq)

instance ShowToUser Piece where
	showToUser =  showToUser . grid

color :: Piece -> Color
color (Piece (Grid array width)) = head $ filter (/= Empty) array

rotations :: Piece -> [Piece]
rotations (Piece grid) = map (Piece) (nub $ map ($ grid) [id, 
											 			  rotate90,
											 			  rotate180, 
											 			  rotate270, 
											 			  flipAboutVertical, 
											 			  flipAboutVertical . rotate90, 
											 			  flipAboutVertical . rotate180, 
											 			  flipAboutVertical . rotate270])

allPoints = Grid.allPoints . grid

filledPoints :: Piece -> [Point]
filledPoints (Piece grid) = filter (\point -> itemAt grid point /= Empty) (Grid.allPoints grid)