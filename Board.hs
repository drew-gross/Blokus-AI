module Board(
	Board(Board, grid, startPoints),
	empty2PlayerBoard,
	colorAt,
	changeColorAt,
	isPointInBounds,
	isPointAdjacentToColor,
	isPointCornerToColor,
	isPointLaunchPointForColor,
	numOfLaunchPointsForColor,
	displayString
) where

import Debug.Trace
import Data.List.Split
import Data.Maybe

import Grid
import Color
import Point
import Display
import Utilities
import Piece

data Board = Board {grid :: Grid Color, startPoints :: [Point]}

defaultSize = 14
defaultStartPoints = [Point 4 4, Point 9 9]

empty2PlayerBoard :: Board
empty2PlayerBoard = Board (makeEmptyGrid defaultSize defaultSize Empty) defaultStartPoints

colorAt :: Board -> Point -> Color
colorAt (Board grid _) point
	| containsPoint grid point == False = error "colorAt: point outside of board"
	| otherwise = fromJust $ itemAt grid point

changeColorAt :: Board -> Color -> Point -> Board
changeColorAt (Board grid startPoints) color point = Board (changeItemAt grid color point) startPoints

cornersOfPoint :: Board -> Point -> [Color]
cornersOfPoint (Board grid _) point
	| not $ containsPoint grid point = error "index out of range"
	| otherwise = catMaybes items
	where
		points = map ($ point) [ulPoint, urPoint, drPoint, dlPoint]
		items = map (itemAt grid) points

sidesOfPoint :: Board -> Point -> [Color]
sidesOfPoint (Board grid _) point
	| not $ containsPoint grid point = error "index out of range"
	| otherwise = catMaybes items
	where
		points = map ($ point) [leftPoint, rightPoint, upPoint, downPoint]
		items = map (itemAt grid) points

isPointAdjacentToColor :: Board -> Color -> Point -> Bool
isPointAdjacentToColor board color point = color `elem` sidesOfPoint board point

isPointCornerToColor :: Board -> Color -> Point -> Bool
isPointCornerToColor board color point = color `elem` cornersOfPoint board point

isPointLaunchPointForColor :: Board -> Color -> Point -> Bool
isPointLaunchPointForColor board color point 
	| not $ isPointInBounds board point = False
	| colorAtPoint /= Empty = False
	| isPointAdjacentToColor board color point = False
	| isPointCornerToColor board color point = True
	| point `elem` startPoints board = True
	| otherwise = False
	where colorAtPoint = colorAt board point

launchPointsForColor :: Board -> Color -> [Point]
launchPointsForColor board@(Board grid _) color = filter (isPointLaunchPointForColor board color) $ allPoints grid

numOfLaunchPointsForColor :: Board -> Color -> Int
numOfLaunchPointsForColor board = length . (launchPointsForColor board)

prevPoint :: Piece -> Point -> Point
prevPoint (Piece grid _) (Point 0 y) = Point (width grid - 1) (y - 1)
prevPoint _ point = Point (x point - 1) (y point)

isPointInBounds :: Board -> Point -> Bool
isPointInBounds (Board grid _) (Point x y)
	| x < 0 = False
	| y < 0 = False
	| x >= width grid = False
	| y >= height grid = False
	| otherwise = True

displayString :: Board -> Color -> Point -> String
displayString board color point
	| isPointInBounds board point == False = error "displayChar: point out of bounds of board"
	| colorAtPoint /= Empty = display colorAtPoint
	| isPointLaunchPointForColor board color point = "O"
	| otherwise = "."
	where colorAtPoint = colorAt board point