module Player(
	Player(Player, color, pieces),
	doTurn,
	removePiece,
	startingPieces,
	getRotatedPiece,
	displayToUserForPlayer,
	winner
) where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Function

import Color
import Grid
import Display
import Piece
import Utilities
import Board
import Point

data Player = Player {pieces :: [Piece], color :: Color, completeMove :: Player -> Board -> IO (Maybe (Board, Player))}

instance Display Player where
	display player = let
		pieceAnnotations = ["Piece " ++ show num ++ ":\n" | num <- [1..]]
		pieceStrings = map display $ pieces player
		in concat $ zipWith (++) pieceAnnotations pieceStrings

removePiece :: Player -> Piece -> Player
removePiece (Player pieces color doTurn) piece = Player (pieces \\ [piece]) color doTurn

startingGrids color = 
			 [
			 makeFilledGridWithList [color] 1, --1

			 makeFilledGridWithList [color, color] 2, --2

			 makeFilledGridWithList [color, color, color] 3, --I3
			 makeFilledGridWithList [color, Empty, color, color] 2, --V3

			 makeFilledGridWithList [color, color, color, color] 4, --I4
			 makeFilledGridWithList [color, color, color, Empty, color, Empty] 3, --T4
			 makeFilledGridWithList [color, color, color, Empty, Empty, color] 3, --L4
			 makeFilledGridWithList [color, color, Empty, Empty, color, color] 3, --Z4
			 makeFilledGridWithList [color, color, color, color] 2, --O

			 makeFilledGridWithList [color, color, color, color, color] 5, --I5
			 makeFilledGridWithList [color, color, color, color, color, Empty, Empty, Empty] 4, --L5
			 makeFilledGridWithList [color, color, color, color, Empty, color, Empty, Empty] 4, --Y
			 makeFilledGridWithList [Empty, color, color, color, color, color, Empty, Empty] 4, --N
			 makeFilledGridWithList [color, color, color, color, color, Empty] 3, --P
			 makeFilledGridWithList [color, color, color, color, Empty, color] 3, --U
			 makeFilledGridWithList [Empty, color, Empty, color, color, color, Empty, color, Empty] 3, --X
			 makeFilledGridWithList [color, color, color, color, Empty, Empty, color, Empty, Empty] 3, --V5
			 makeFilledGridWithList [color, color, color, Empty, color, Empty, Empty, color, Empty] 3, --T5
			 makeFilledGridWithList [color, Empty, Empty, color, color, color, Empty, Empty, color] 3, --Z5
			 makeFilledGridWithList [color, Empty, Empty, color, color, Empty, Empty, color, color] 3, --W
			 makeFilledGridWithList [Empty, color, color, color, color, Empty, Empty, color, Empty] 3 --F
			 ]
	
startingPieces color = zipWith (Piece) (startingGrids color) $ [1..]

getUnrotatedPiece :: Player -> Board -> IO Piece
getUnrotatedPiece player@(Player pieces _ _) board = do
	maybePiece <- fmap (maybeIndex pieces) $ fmap read1IndexedIndex $ prompt promptString
	fromMaybe (getUnrotatedPiece player board) $ fmap return maybePiece
	where
		promptString = displayToUserForPlayer player board ++ "\n" ++ (display player) ++ "\n" ++ "Enter piece number: "

getRotatedPiece :: Player -> Board -> IO Piece
getRotatedPiece player board = do
	rotatedPieceList <- fmap rotations $ getUnrotatedPiece player board
	let promptString = (displayNumberedList $ rotatedPieceList) ++ "\n" ++ "Enter rotation number:"
	maybeRotatedPiece <- fmap (maybeIndex rotatedPieceList) $ fmap read1IndexedIndex $ prompt promptString
	fromMaybe (getRotatedPiece player board) $ fmap return maybeRotatedPiece

displayForPlayer :: Player -> Board -> String
displayForPlayer (Player _ color _) board@(Board grid sp) = unlines $ concat splitChars
	where
		strings = map (displayString board color) (range origin $ maxPoint grid)
		splitChars = chunksOf (width grid) strings 

displayToUserForPlayer :: Player -> Board -> String
displayToUserForPlayer player board = (++) " 12345678901234\n" $ unlines $ zipWith (++) (map show repeatedSingleDigits) (lines $ displayForPlayer player board)

doTurn :: Player -> Board -> IO (Maybe (Board, Player))
doTurn player board = completeMove player player board

squaresRemaining :: Player -> Int
squaresRemaining (Player pieces _ _)= sum $ map filledPointsCount pieces

winner :: [Player] -> Player
winner players = head $ sortBy (compare `on` squaresRemaining) players