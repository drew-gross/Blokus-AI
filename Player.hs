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
			 Grid [color] 1, --1

			 Grid [color, color] 2, --2

			 Grid [color, color, color] 3, --I3
			 Grid [color, Empty, color, color] 2, --V3

			 Grid [color, color, color, color] 4, --I4
			 Grid [color, color, color, Empty, color, Empty] 3, --T4
			 Grid [color, color, color, Empty, Empty, color] 3, --L4
			 Grid [color, color, Empty, Empty, color, color] 3, --Z4
			 Grid [color, color, color, color] 2, --O

			 Grid [color, color, color, color, color] 5, --I5
			 Grid [color, color, color, color, color, Empty, Empty, Empty] 4, --L5
			 Grid [color, color, color, color, Empty, color, Empty, Empty] 4, --Y
			 Grid [Empty, color, color, color, color, color, Empty, Empty] 4, --N
			 Grid [color, color, color, color, color, Empty] 3, --P
			 Grid [color, color, color, color, Empty, color] 3, --U
			 Grid [Empty, color, Empty, color, color, color, Empty, color, Empty] 3, --X
			 Grid [color, color, color, color, Empty, Empty, color, Empty, Empty] 3, --V5
			 Grid [color, color, color, Empty, color, Empty, Empty, color, Empty] 3, --T5
			 Grid [color, Empty, Empty, color, color, color, Empty, Empty, color] 3, --Z5
			 Grid [color, Empty, Empty, color, color, Empty, Empty, color, color] 3, --W
			 Grid [Empty, color, color, color, color, Empty, Empty, color, Empty] 3 --F
			 ]
	
startingPieces color = 
	[Piece grid identifier | (grid, identifier) <- pairs]
	where
		pairs = zip (startingGrids color) [1..]

newPlayer :: Color -> Player
newPlayer color = Player (startingPieces color) color