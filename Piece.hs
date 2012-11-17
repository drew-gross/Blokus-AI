module Piece(
	Piece(Piece, grid),
	rotations,
	Piece.color,
	filledPoints,
	filledPointsCount
) where

import Control.Applicative

import Data.List

import Point
import Grid
import Color
import Display

data Piece = Piece {grid :: Grid Color, identifier :: Int} deriving (Show)

instance Display Piece where
	display =  display . grid

instance Eq Piece where
	(==) left right = Piece.color left == Piece.color right && identifier left == identifier right
	(/=) left right = not $ left == right

color :: Piece -> Color
color (Piece grid _) = Grid.color grid

pieceWithID = flip Piece

rotations :: Piece -> [Piece]
rotations (Piece grid identifier) = pieceWithID identifier <$> (nub $ ($ grid) <$> [
														  id, 
											 			  rotate90,
											 			  rotate180, 
											 			  rotate270, 
											 			  flipAboutVertical, 
											 			  flipAboutVertical . rotate90, 
											 			  flipAboutVertical . rotate180, 
											 			  flipAboutVertical . rotate270])	

filledPoints :: Piece -> [Point]
filledPoints (Piece grid _) = filter (\point -> unsafeItemAt grid point /= Empty) (allPoints grid)

filledPointsCount :: Piece -> Int
filledPointsCount = length . filledPoints