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
rotations (Piece grid) = map (Piece) (nub $ map ($ grid) [id, 
											 			  rotate90,
											 			  rotate180, 
											 			  rotate270, 
											 			  flipAboutVertical, 
											 			  flipAboutVertical . rotate90, 
											 			  flipAboutVertical . rotate180, 
											 			  flipAboutVertical . rotate270])