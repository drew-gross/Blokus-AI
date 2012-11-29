module Move(
	Move(Move, piece, position),
	apply,
	isValid,
	filledPointsOnBoard
) where

import Control.Applicative

import Data.Maybe
import Data.Function
import Data.List

import Utilities
import Piece
import Point
import Board
import Grid
import Color

data Move = Move {piece :: Piece, board :: Board, position :: Point}

apply :: Move -> Board
apply move@(Move piece board position) = modifiedBoard 
	where
		modifiedBoard = foldl (changeColorAt' $ Piece.color piece) board $ filledPointsOnBoard move
		changeColorAt' :: Color -> Board -> Point -> Board
		changeColorAt' color board = changeColorAt board color

filledPointsOnBoard :: Move -> [Point]
filledPointsOnBoard (Move piece _ position) = plus position <$> filledPoints piece

isInBounds :: Move -> Bool
isInBounds (Move (Piece grid _) board position)
	| not $ containsPoint (Board.grid board) position = False
	| not $ containsPoint (Board.grid board) $ position `plus` (maxPoint grid) = False
	| otherwise = True

isValid :: Move -> Bool
isValid move@(Move piece board position)
	| not $ isInBounds move = False --move is outside of board
	| any (\point -> unsafeColorAt board point /= Empty) pointsOnBoard = False --move is overlapping another piece
	| any (isPointAdjacentToColor board color) pointsOnBoard = False --side of piece is touching its own color
	| any (isPointLaunchPointForColor board color) pointsOnBoard = True
	| otherwise = False
	where
		color = Piece.color piece
		pointsOnBoard = plus position <$> filledPoints piece