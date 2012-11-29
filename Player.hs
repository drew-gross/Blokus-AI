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

data Player = Player {pieces :: [Piece], color :: Color, completeMove :: Player -> Board -> Player -> IO (Maybe (Board, Player)), name :: String}

instance Display Player where
	display player = let
		pieceAnnotations = ["Piece " ++ show num ++ ":\n" | num <- [1..]]
		pieceStrings = display <$> pieces player
		in concat $ zipWith (++) pieceAnnotations pieceStrings

instance Eq Player where
	(==) left right = name left == name right
	(/=) left right = not $ left == right

candidateMovesForPieceRotation :: Board -> Piece -> [Move]
candidateMovesForPieceRotation board@(Board boardGrid _) piece@(Piece pieceGrid identifier) = let
		maxPlacementPoint = ((maxPoint boardGrid) `minus` (maxPoint pieceGrid))
	in Move piece board <$> range origin maxPlacementPoint

validMovesForPieceRotation :: Board -> Piece -> [Move]
validMovesForPieceRotation board = (filter isValid) . (candidateMovesForPieceRotation board)

validMovesForPiece :: Board -> Piece -> [Move]
validMovesForPiece board piece = concat $ validMovesForPieceRotation board <$> rotations piece

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

getUnrotatedPiece :: Player -> Board -> IO Piece
getUnrotatedPiece player@(Player pieces _ _ _) board = do
	maybePiece <- maybeIndex pieces <$> read1IndexedIndex <$> prompt promptString
	fromMaybe (getUnrotatedPiece player board) $ return <$> maybePiece
	where
		promptString = displayToUserForPlayer player board ++ "\n" ++ (display player) ++ "\n" ++ "Enter piece number: "

getRotatedPiece :: Player -> Board -> IO Piece
getRotatedPiece player board = do
	rotatedPieceList <- rotations <$> getUnrotatedPiece player board
	let promptString = (displayNumberedList $ rotatedPieceList) ++ "\n" ++ "Enter rotation number:"
	maybeRotatedPiece <- maybeIndex rotatedPieceList <$> read1IndexedIndex <$> prompt promptString
	fromMaybe (getRotatedPiece player board) $ return <$> maybeRotatedPiece

getMove :: Player -> Board -> Player -> IO (Maybe (Move, Board, Player))
getMove player board _ = do
	piece <- getRotatedPiece player board
	move <- Move <$> (return piece) <*> (return board) <*> read1IndexedPoint <$> getPoint
	return $ Just (move, apply move, removePiece player piece)

validMoves :: Player -> Board -> [Move]
validMoves (Player pieces _ _ _) board = concat $ validMovesForPiece board <$> pieces

displayForPlayer :: Player -> Board -> String
displayForPlayer (Player _ color _ _) board@(Board grid _) = unlines $ concat splitChars
	where
		strings = displayString board color <$> (allPoints grid)
		splitChars = chunksOf (width grid) strings 

displayToUserForPlayer :: Player -> Board -> String
displayToUserForPlayer player board = (++) " 12345678901234\n" $ unlines $ zipWith (++) (show <$> repeatedSingleDigits) (lines $ displayForPlayer player board)

doTurn :: Player -> Board -> Player -> IO (Maybe (Board, Player))
doTurn player@(Player _ _ completeMove _) = completeMove player

squaresRemaining :: Player -> Int
squaresRemaining (Player pieces _ _ _)= sum $ filledPointsCount <$> pieces

winner :: [Player] -> Player
winner = head . sortBy (compare `on` squaresRemaining)