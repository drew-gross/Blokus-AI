module Player(
	Player(Player),
	pieces,
	removePiece
) where

import Color
import Grid
import Utilities

type Piece = Grid Color

data Player = Player {pieces :: [Piece], color :: Color} deriving (Show)

instance ShowToUser Player where
	showToUser player = let
		pieceAnnotations = ["Piece " ++ show num ++ ":\n" | num <- [1..]]
		pieceStrings = map showToUser (pieces player)
		pairs = zip pieceAnnotations pieceStrings
		in concat $ map (\pair -> fst pair ++ snd pair) pairs

removePiece :: Player -> Int -> Player
removePiece (Player pieces color) pieceIndex = Player (removeItem pieceIndex pieces) color