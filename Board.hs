module Board(
	Board(Board, grid, startPoints),
	empty2PlayerBoard,
	unsafeColorAt,
	changeColorAt,
	isPointAdjacentToColor,
	isPointCornerToColor,
	isPointLaunchPointForColor,
	numOfLaunchPointsForColor,
	displayString
) where

import Control.Applicative

import Data.List.Split
import Data.Maybe

import Grid
import Color
import Point
import Display
import Utilities
import Piece

data Board = Board {grid :: Grid Color, startPoints :: [Point]} deriving (Show)

defaultSize = 14
defaultStartPoints = [Point 4 4, Point 9 9]

empty2PlayerBoard :: Board
empty2PlayerBoard = Board (makeEmptyGrid defaultSize defaultSize Empty) defaultStartPoints

colorAt :: Board -> Point -> Maybe Color
colorAt (Board grid _) point = itemAt grid point

unsafeColorAt :: Board -> Point -> Color
unsafeColorAt (Board grid _) point
	| isNothing item = error $ "unsafeColorAt has a Nothing as its item!\n" ++ show grid
	| otherwise = fromJust $ item
	where
		item = itemAt grid point

changeColorAt :: Board -> Color -> Point -> Board
changeColorAt (Board grid startPoints) color point = Board (changeItemAt grid color point) startPoints

cornersOfPoint :: Board -> Point -> [Color]
cornersOfPoint (Board grid _) point = catMaybes items
	where
		points = ($ point) <$> [ulPoint, urPoint, drPoint, dlPoint]
		items = itemAt grid <$> points

sidesOfPoint :: Board -> Point -> [Color]
sidesOfPoint (Board grid _) point = catMaybes items
	where
		points = ($ point) <$> [leftPoint, rightPoint, upPoint, downPoint]
		items = itemAt grid <$> points

isPointAdjacentToColor :: Board -> Color -> Point -> Bool
isPointAdjacentToColor board color point = color `elem` sidesOfPoint board point

isPointCornerToColor :: Board -> Color -> Point -> Bool
isPointCornerToColor board color point = color `elem` cornersOfPoint board point

isPointLaunchPointForColor :: Board -> Color -> Point -> Bool
isPointLaunchPointForColor board color point 
	| isNothing colorAtPoint = False
	| fromJust colorAtPoint /= Empty = False
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

displayString :: Board -> Color -> Point -> String
displayString board color point
	| isNothing colorAtPoint = error "displayChar: point out of bounds of board"
	| fromJust colorAtPoint /= Empty = display $ fromJust colorAtPoint
	| isPointLaunchPointForColor board color point = "O"
	| otherwise = "."
	where colorAtPoint = colorAt board point