module Player(
	Player(Player, color, pieces),
	piecesWithIndices,
	removePiece,
	newPlayer
) where

import Color
import Grid
import Display
import Piece
import Utilities

data Player = Player {pieces :: [Piece], color :: Color} deriving (Show)

instance Display Player where
	display player = let
		pieceAnnotations = ["Piece " ++ show num ++ ":\n" | num <- [1..]]
		pieceStrings = map display (pieces player)
		pairs = zip pieceAnnotations pieceStrings
		in concat $ map (\pair -> fst pair ++ snd pair) pairs

removePiece :: Player -> Int -> Player
removePiece (Player pieces color) pieceIndex = Player (removeItem pieceIndex pieces) color

piecesWithIndices :: Player -> [(Piece, Int)]
piecesWithIndices player = zip (pieces player) [0..]

newPlayer :: Color -> Player
newPlayer color = (Player 
			(
			 [
			 Piece $ Grid [color, color, color, color, color, Empty] 3,
			 Piece $ Grid [color] 1, 
			 Piece $ Grid [color, Empty, color, color] 2,
			 Piece $ Grid [color, color] 1,
			 Piece $ Grid [color, color, color, color] 2,
			 Piece $ Grid [color, color, color] 1
			 ]
			)
			 color)