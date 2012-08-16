module Board(
	Board(Board, grid, startPoints),
	displayChar,
	displayForPlayer
) where

import Data.List.Split

import Grid
import Color
import Point
import Player
import Utilities

data Board = Board {grid :: Grid Color, startPoints :: [Point]}

instance ShowToUser Board where
	showToUser = showToUser . grid

cornersOfPoint :: Board -> Point -> [Color]
cornersOfPoint (Board grid _) point
	| not $ containsPoint grid point = error "index out of range"
	| otherwise = 	(safeItemAt grid (ulPoint point)) ++ 
					(safeItemAt grid (urPoint point)) ++ 
					(safeItemAt grid (drPoint point)) ++ 
					(safeItemAt grid (dlPoint point))

sidesOfPoint :: Board -> Point -> [Color]
sidesOfPoint (Board grid _) point
	| not $ containsPoint grid point = error "index out of range"
	| otherwise = 	(safeItemAt grid $ leftPoint point) ++ 
					(safeItemAt grid $ rightPoint point) ++ 
					(safeItemAt grid $ upPoint point) ++ 
					(safeItemAt grid $ downPoint point)

isPointOpenToColor :: Board -> Color -> Point -> Bool
isPointOpenToColor board color (Point 0 0) = itemAt (grid board) (Point 0 0) == Empty
isPointOpenToColor board color point = (color `elem` (cornersOfPoint board point)) && 
												(not (color `elem` sidesOfPoint board point)) && 
												((itemAt (grid board) point) == Empty)

displayChar :: Board -> Color -> Point -> Char
displayChar board color point
	| itemAt (grid board) point == Red =     'R'
	| itemAt (grid board) point == Green =   'G'
	| itemAt (grid board) point == Blue =    'B'
	| itemAt (grid board) point == Yellow =  'Y'
	| isPointOpenToColor board color point = 'O'
	| otherwise = '.'

displayForPlayer :: Board -> Player -> String
displayForPlayer board player = let
	chars = map (displayChar board (color player)) (range (Point 0 0) (maxPoint $ grid board))
	splitChars = splitEvery (width $ grid board) chars 
	in unlines splitChars