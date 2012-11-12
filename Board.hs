module Board(
	Board(Board, grid, startPoints),
	applyMove,
	isMoveValid,
	validMovesForPiece,
	empty2PlayerBoard,
	displayChar
) where

import Debug.Trace
import Data.List.Split
import Data.Maybe

import Grid
import Color
import Point
import Display
import Utilities
import Move
import Piece

data Board = Board {grid :: Grid Color, startPoints :: [Point]}

defaultSize = 14
defaultStartPoints = [Point 4 4, Point 9 9]

empty2PlayerBoard :: Board
empty2PlayerBoard = Board (Grid (take (defaultSize * defaultSize) $ repeat Empty) defaultSize) defaultStartPoints

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

isMoveValid :: Board -> Move -> Bool
isMoveValid board move
	| not $ isMoveInBounds board move = False --move is outside of board
	| any (\point -> colorAt board point /= Empty) pointsOnBoard = False --move is overlapping another piece
	| any (isPointAdjacentToColor board color) pointsOnBoard = False --side of piece is touching its own color
	| any (isPointLaunchPointForColor board color) pointsOnBoard = True
	| otherwise = False
	where
		color = Piece.color $ piece move
		pointsInPiece = filledPoints $ piece move
		pointsOnBoard = map (plus $ position move) pointsInPiece

prevPoint :: Piece -> Point -> Point
prevPoint (Piece grid _) (Point 0 y) = Point (width grid - 1) (y - 1)
prevPoint _ point = Point (x point - 1) (y point)

applyMove :: Board -> Move -> Board
applyMove board (Move piece position) = foldl (changeColorAt' $ Piece.color piece) board pointsOnBoard
	where
		pointsOnBoard = map (plus position) $ filledPoints piece
		changeColorAt' :: Color -> Board -> Point -> Board
		changeColorAt' color board = changeColorAt board color

isMoveInBounds :: Board -> Move -> Bool
isMoveInBounds board (Move piece position)
	| not $ isPointInBounds board position = False
	| not $ isPointInBounds board $ position `plus` (maxPoint $ Piece.grid piece) = False
	| otherwise = True

candidateMovesForPieceRotation :: Board -> Piece -> [Move]
candidateMovesForPieceRotation (Board boardGrid _) (Piece pieceGrid identifier) = let
		maxPlacementPoint = ((maxPoint boardGrid) `minus` (maxPoint pieceGrid))
	in map (Move (Piece pieceGrid identifier)) (range origin maxPlacementPoint)

validMovesForPieceRotation :: Board -> Piece -> [Move]
validMovesForPieceRotation board piece = filter (isMoveValid board) (candidateMovesForPieceRotation board piece)

validMovesForPiece :: Board -> Piece -> [Move]
validMovesForPiece board piece = concatMap (validMovesForPieceRotation board) (rotations piece)

isPointInBounds :: Board -> Point -> Bool
isPointInBounds (Board grid _) (Point x y)
	| x < 0 = False
	| y < 0 = False
	| x >= width grid = False
	| y >= height grid = False
	| otherwise = True

displayChar :: Board -> Color -> Point -> Char
displayChar board color point
	| isPointInBounds board point == False = error "displayChar: point out of bounds of board"
	| colorAt board point == Red =     'R'
	| colorAt board point == Green =   'G'
	| colorAt board point == Blue =    'B'
	| colorAt board point == Yellow =  'Y'
	| isPointLaunchPointForColor board color point = 'O'
	| otherwise = '.'