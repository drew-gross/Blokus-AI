module Player(
	Player(Player, color, pieces),
	removePiece,
	newPlayer
) where

import Data.List

import Color
import Grid
import Display
import Piece
import Utilities

data Player = Player {pieces :: [Piece], color :: Color} deriving (Show)

instance Display Player where
	display player = let
		pieceAnnotations = ["Piece " ++ show num ++ ":\n" | num <- [1..]]
		pieceStrings = map display $ pieces player
		pairs = zip pieceAnnotations pieceStrings
		in concat $ map (\(first, second) -> first ++ second) pairs

removePiece :: Player -> Piece -> Player
removePiece (Player pieces color) piece = Player (pieces \\ [piece]) color

startingGrids color = 
			 [
			 Grid [color, color, color, color, color, Empty] 3,
			 Grid [color] 1, 
			 Grid [color, Empty, color, color] 2,
			 Grid [color, color] 1,
			 Grid [color, color, color, color] 2,
			 Grid [color, color, color] 1
			 ]
	
startingPieces color = 
	[Piece grid identifier | (grid, identifier) <- pairs]
	where
		pairs = zip (startingGrids color) [1..]

newPlayer :: Color -> Player
newPlayer color = Player (startingPieces color) color