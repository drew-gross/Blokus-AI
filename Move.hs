module Move(
	Move(Move, piece, position),
	filledPointsOnBoard
) where

import Control.Applicative

import Data.Maybe
import Data.Function
import Data.List

import Utilities
import Piece
import Point
import Grid
import Color

data Move = Move {piece :: Piece, position :: Point}

filledPointsOnBoard :: Move -> [Point]
filledPointsOnBoard (Move piece position) = plus position <$> filledPoints piece