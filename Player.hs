module Player(
	Player(Player, color, pieces, name),
	doTurn,
	removePiece,
	startingPieces,
	getRotatedPiece,
	displayToUserForPlayer,
	winner,
	getMove,
	validMoves
) where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

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
import Move

data Player = Player {pieces :: [Piece], color :: Color, completeMove :: Player -> Board -> Player -> MaybeT IO (Board, Player), name :: String}

instance Display Player where
	display player = let
		pieceAnnotations = ["Piece " ++ show num ++ ":\n" | num <- [1..]]
		pieceStrings = display <$> pieces player
		in concat $ zipWith (++) pieceAnnotations pieceStrings

instance Show Player where
	show = name

instance Eq Player where
	(==) left right = name left == name right
	(/=) left right = not $ left == right

removePiece :: Player -> Piece -> Player
removePiece (Player pieces color doTurn name) piece = Player (pieces \\ [piece]) color doTurn name

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

getUnrotatedPiece :: Player -> Board -> MaybeT IO Piece
getUnrotatedPiece player@(Player pieces _ _ _) board = do
	maybePiece <- maybeIndex pieces <$> read1IndexedIndex <$> (lift $ prompt promptString)
	fromMaybe (getUnrotatedPiece player board) $ return <$> maybePiece
	where
		promptString = displayToUserForPlayer player board ++ "\n" ++ (display player) ++ "\n" ++ "Enter piece number: "

getRotatedPiece :: Player -> Board -> MaybeT IO Piece
getRotatedPiece player board = do
	rotatedPieceList <- rotations <$> getUnrotatedPiece player board
	let promptString = (displayNumberedList $ rotatedPieceList) ++ "\n" ++ "Enter rotation number:"
	maybeRotatedPiece <- lift $ maybeIndex rotatedPieceList <$> read1IndexedIndex <$> prompt promptString
	fromMaybe (getRotatedPiece player board) $ return <$> maybeRotatedPiece

getMove :: Player -> Board -> Player -> MaybeT IO (Move, Board, Player)
getMove player board _ = do
	piece <- getRotatedPiece player board
	move <- Move <$> (return piece) <*> read1IndexedPoint <$> getPoint
	lift $ return (move, applyMove board move, removePiece player piece)

validMoves :: Player -> Board -> [Move]
validMoves (Player pieces _ _ _) board = concat $ validMovesForPiece board <$> pieces

displayForPlayer :: Player -> Board -> String
displayForPlayer (Player _ color _ _) board@(Board grid _) = unlines splitChars
	where
		pointRows = chunksOf (width grid) (range origin $ maxPoint grid)
		strings = concatMap (displayString board color) <$> pointRows
		splitChars = concat $ chunksOf (width grid) strings 

displayToUserForPlayer :: Player -> Board -> String
displayToUserForPlayer player board = header ++ annotatedBoardString
	where
		header = " 12345678901234\n"
		boardString = displayForPlayer player board
		annotatedBoardString = unlines $ zipWith (++) (show <$> repeatedSingleDigits) (lines $ boardString)

doTurn :: Player -> Board -> Player -> MaybeT IO (Board, Player)
doTurn player@(Player _ _ completeMove _) = completeMove player

squaresRemaining :: Player -> Int
squaresRemaining (Player pieces _ _ _)= sum $ length <$> filledPoints <$> pieces

winner :: [Player] -> Player
winner = head . sortBy (compare `on` squaresRemaining)
