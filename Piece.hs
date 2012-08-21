module Piece(
	Piece(Piece, grid),
	rotations,
	color,
	filledPoints,
) where

import Data.List

import Point
import Grid
import Color
import Display

data Piece = Piece {grid :: Grid Color} deriving (Show, Eq)

instance Display Piece where
	display =  display . grid

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

filledPoints :: Piece -> [Point]
filledPoints (Piece grid) = filter (\point -> itemAt grid point /= Empty) (allPoints grid)