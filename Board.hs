module Board(
	Board(Board, grid, startPoints),
	empty2PlayerBoard,
	unsafeColorAt,
	changeColorAt,
	isPointAdjacentToColor,
	isPointCornerToColor,
	isPointLaunchPointForColor,
	numOfLaunchPointsForColor,
	displayString,
	isValid,
	applyMove,
	validMovesForPiece
) where

import Control.Applicative hiding (empty)

import Data.List.Split
import Data.Maybe

import Grid
import Color
import Point
import Display
import Utilities
import Piece
import Move

data Board = Board {grid :: Grid Color, startPoints :: [Point]} deriving (Show)

defaultSize = 14
defaultStartPoints = [Point 4 4, Point 9 9]

empty2PlayerBoard :: Board
empty2PlayerBoard = Board (makeEmptyGrid defaultSize defaultSize empty) defaultStartPoints

candidateMovesForPieceRotation :: Board -> Piece -> [Move]
candidateMovesForPieceRotation board@(Board boardGrid _) piece@(Piece pieceGrid identifier) = Move piece <$> range origin maxPlacementPoint
	where
		maxPlacementPoint = ((maxPoint boardGrid) `minus` (maxPoint pieceGrid))

validMovesForPieceRotation :: Board -> Piece -> [Move]
validMovesForPieceRotation board = (filter $ isValid board) . (candidateMovesForPieceRotation board)

validMovesForPiece :: Board -> Piece -> [Move]
validMovesForPiece board piece = concat $ validMovesForPieceRotation board <$> rotations piece

isInBounds :: Board -> Move -> Bool
isInBounds (Board bgrid _) (Move (Piece grid _) position)
	| not $ containsPoint bgrid position = False
	| not $ containsPoint bgrid $ position `plus` (maxPoint grid) = False
	| otherwise = True

isValid :: Board -> Move -> Bool
isValid board move@(Move piece position)
	| not $ isInBounds board move = False --move is outside of board
	| any (\point -> unsafeColorAt board point /= empty) pointsOnBoard = False --move is overlapping another piece
	| any (isPointAdjacentToColor board color) pointsOnBoard = False --side of piece is touching its own color
	| any (isPointLaunchPointForColor board color) pointsOnBoard = True
	| otherwise = False
	where
		color = Piece.color piece
		pointsOnBoard = plus position <$> filledPoints piece

applyMove :: Board -> Move -> Board
applyMove board move@(Move piece position) = modifiedBoard 
	where
		modifiedBoard = foldl (changeColorAt' $ Piece.color piece) board $ filledPointsOnBoard move
		changeColorAt' :: Color -> Board -> Point -> Board
		changeColorAt' color board = changeColorAt board color

colorAt :: Board -> Point -> Maybe Color
colorAt (Board grid _) = itemAt grid

unsafeColorAt :: Board -> Point -> Color
unsafeColorAt (Board grid _) point = result item
	where
		item = itemAt grid point
		result Nothing = error $ "unsafeColorAt has a Nothing as its item!\n" ++ show grid
		result (Just val) = val

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
	| fromJust colorAtPoint /= empty = False
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
	| fromJust colorAtPoint /= empty = display $ fromJust colorAtPoint
	| isPointLaunchPointForColor board color point = "O"
	| otherwise = "."
	where colorAtPoint = colorAt board point