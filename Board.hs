module Board(
	Board(Board, grid, startPoints),
	displayForPlayer,
	printBoard,
	displayToUserForPlayer,
	isPointValidToColor,
	isPointOpenToColor
) where

import Debug.Trace
import Data.List.Split

import Grid
import Color
import Point
import Player
import Display
import Utilities

data Board = Board {grid :: Grid Color, startPoints :: [Point]}

instance Display Board where
	display = display . grid

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

isPointValidToColor :: Board -> Color -> Point -> Bool
isPointValidToColor board color point = not $ color `elem` sidesOfPoint board point

isPointOpenToColor :: Board -> Color -> Point -> Bool
isPointOpenToColor board color point 
	| point `elem` startPoints board && itemAt (grid board) point == Empty = True
	| otherwise = (color `elem` (cornersOfPoint board point)) && 
				  isPointValidToColor board color point && 
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
	chars = map (displayChar board (color player)) (range origin (maxPoint $ grid board))
	splitChars = splitEvery (width $ grid board) chars 
	in unlines splitChars

printBoard board = putStr . (displayForPlayer board)

displayToUserForPlayer :: Board -> Player -> String
displayToUserForPlayer board player = " 12345678901234\n" ++ unlines (map concatTuple (zip (map show repeatedSingleDigits) (lines $ displayForPlayer board player)))