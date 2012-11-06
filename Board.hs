module Board(
	Board(Board, grid, startPoints),
	displayForPlayer,
	printBoard,
	displayToUserForPlayer,
	isPointAdjacentToColor,
	isMoveValid,
	empty2PlayerBoard
) where

import Debug.Trace
import Data.List.Split

import Grid
import Color
import Point
import Player
import Display
import Utilities
import Move
import Piece

data Board = Board {grid :: Grid Color, startPoints :: [Point]}

instance Display Board where
	display = display . Board.grid

defaultSize = 14
defaultStartPoints = [Point 4 4, Point 9 9]

empty2PlayerBoard :: Board
empty2PlayerBoard = Board (Grid (take (defaultSize * defaultSize) $ repeat Empty) defaultSize) defaultStartPoints

colorAt :: Board -> Point -> Color
colorAt (Board grid _) point = itemAt grid point

cornersOfPoint :: Board -> Point -> [Color]
cornersOfPoint (Board grid _) point
	| not $ containsPoint grid point = error "index out of range"
	| otherwise = 	(safeItemAt grid $ ulPoint point) ++ 
					(safeItemAt grid $ urPoint point) ++ 
					(safeItemAt grid $ drPoint point) ++ 
					(safeItemAt grid $ dlPoint point)

sidesOfPoint :: Board -> Point -> [Color]
sidesOfPoint (Board grid _) point
	| not $ containsPoint grid point = error "index out of range"
	| otherwise = 	(safeItemAt grid $ leftPoint point) ++ 
					(safeItemAt grid $ rightPoint point) ++ 
					(safeItemAt grid $ upPoint point) ++ 
					(safeItemAt grid $ downPoint point)

isPointAdjacentToColor :: Board -> Color -> Point -> Bool
isPointAdjacentToColor board color point = not $ color `elem` sidesOfPoint board point

isPointOpenToColor :: Board -> Color -> Point -> Bool
isPointOpenToColor board color point 
	| point `elem` startPoints board && colorAt board point == Empty = True
	| otherwise = (color `elem` (cornersOfPoint board point)) && 
				  isPointAdjacentToColor board color point && 
				  (colorAt board point) == Empty

isMoveValid :: Board -> Move -> Bool
isMoveValid board move
	| not $ isMoveInBounds board move = False
	| not $ and $ map (isPointAdjacentToColor board color) pointsOnBoard = False
	| any (isPointOpenToColor board color) pointsOnBoard = True
	| otherwise = False
	where
		color = Piece.color $ piece move
		pointsInPiece = filledPoints $ piece move
		pointsOnBoard = map (plus $ position move) pointsInPiece

isMoveInBounds :: Board -> Move -> Bool
isMoveInBounds board (Move piece position)
	| x position < 0 = False
	| y position < 0 = False
	| x (position `plus` maxPoint (Piece.grid piece)) >= width (Board.grid board) = False
	| y (position `plus` maxPoint (Piece.grid piece)) >= height (Board.grid board) = False
	| otherwise = True

displayChar :: Board -> Color -> Point -> Char
displayChar board color point
	| colorAt board point == Red =     'R'
	| colorAt board point == Green =   'G'
	| colorAt board point == Blue =    'B'
	| colorAt board point == Yellow =  'Y'
	| isPointOpenToColor board color point = 'O'
	| otherwise = '.'

displayForPlayer :: Board -> Player -> String
displayForPlayer board player = let
	chars = map (displayChar board (Player.color player)) (range origin (maxPoint $ Board.grid board))
	splitChars = splitEvery (width $ Board.grid board) chars 
	in unlines splitChars

printBoard board = putStr . (displayForPlayer board)

displayToUserForPlayer :: Board -> Player -> String
displayToUserForPlayer board player = " 12345678901234\n" ++ unlines (map concatTuple (zip (map show repeatedSingleDigits) (lines $ displayForPlayer board player)))