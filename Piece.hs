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

data Piece = Piece {grid :: Grid Color, identifier :: Int} deriving (Show)

instance Display Piece where
	display =  display . grid

instance Eq Piece where
	(==) left right = color left == color right && identifier left == identifier right
	(/=) left right = not $ left == right

color :: Piece -> Color
color (Piece (Grid array width) _) = head $ filter (/= Empty) array

pieceWithID = flip Piece

rotations :: Piece -> [Piece]
rotations (Piece grid identifier) = map (pieceWithID identifier) (nub $ map ($ grid) [id, 
											 			  rotate90,
											 			  rotate180, 
											 			  rotate270, 
											 			  flipAboutVertical, 
											 			  flipAboutVertical . rotate90, 
											 			  flipAboutVertical . rotate180, 
											 			  flipAboutVertical . rotate270])	

filledPoints :: Piece -> [Point]
filledPoints (Piece grid _) = filter (\point -> itemAt grid point /= Empty) (allPoints grid)