module Player(
	Player(Player, color, pieces),
	doTurn,
	removePiece,
	newHuman,
	newComputer
) where

import Control.Applicative

import Data.List
import Data.List.Split

import Data.Maybe

import Color
import Grid
import Display
import Piece
import Utilities
import Board
import Move
import Point

data Player = Player {pieces :: [Piece], color :: Color, completeMove :: Player -> Board -> IO (Board, Player)}

instance Display Player where
	display player = let
		pieceAnnotations = ["Piece " ++ show num ++ ":\n" | num <- [1..]]
		pieceStrings = map display $ pieces player
		in concat $ zipWith (++) pieceAnnotations pieceStrings

removePiece :: Player -> Piece -> Player
removePiece (Player pieces color doTurn) piece = Player (pieces \\ [piece]) color doTurn

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
	
startingPieces color = zipWith (Piece) (startingGrids color) $ [1..]

allInvalidMovesForPieceRotation :: Board -> Piece -> [Move]
allInvalidMovesForPieceRotation (Board boardGrid _) (Piece pieceGrid identifier) = let
		maxPlacementPoint = ((maxPoint boardGrid) `minus` (maxPoint pieceGrid))
	in map (Move (Piece pieceGrid identifier)) (range origin maxPlacementPoint)

allValidMovesForPieceRotation :: Board -> Piece -> [Move]
allValidMovesForPieceRotation board piece = filter (isMoveValid board) (allInvalidMovesForPieceRotation board piece)

allValidMovesForPiece :: Board -> Piece -> [Move]
allValidMovesForPiece board piece = concatMap (allValidMovesForPieceRotation board) (rotations piece)

allValidMovesForPlayer :: Player -> Board -> [Move]
allValidMovesForPlayer (Player pieces _ _) board = concatMap (allValidMovesForPiece board) pieces

read1IndexdIndex = (flip (-) 1) . read
read1IndexdPoint = (flip minus $ Point 1 1)

getUnrotatedPiece :: Board -> Player -> IO Piece
getUnrotatedPiece board player = do
	maybePiece <- fmap (maybeIndex $ pieces player) $ fmap read1IndexdIndex $ prompt promptString
	fromMaybe (getUnrotatedPiece board player) $ fmap return maybePiece
	where
		promptString = displayToUserForPlayer board player ++ "\n" ++ (display player) ++ "\n" ++ "Enter piece number: "

getRotatedPiece :: Board -> Player -> IO Piece
getRotatedPiece board player = do
	rotatedPieceList <- fmap rotations $ getUnrotatedPiece board player
	let promptString = (displayNumberedList $ rotatedPieceList) ++ "\n" ++ "Enter rotation number:"
	maybeRotatedPiece <- fmap (maybeIndex rotatedPieceList) $ fmap read1IndexdIndex $ prompt promptString
	fromMaybe (getRotatedPiece board player) $ fmap return maybeRotatedPiece

getMove :: Board -> Player -> IO (Move, Board, Player)
getMove board player = do
	piece <- getRotatedPiece board player
	move <- Move <$> (return piece) <*> (fmap read1IndexdPoint getPoint)
	return (move, applyMove board move, removePiece player piece)

displayForPlayer :: Board -> Player -> String
displayForPlayer (Board grid sp) (Player _ color _) = let
	chars = map (displayChar (Board grid sp) color) (range origin $ maxPoint grid)
	splitChars = chunksOf (width grid) chars 
	in unlines splitChars

displayToUserForPlayer :: Board -> Player -> String
displayToUserForPlayer board player = (++) " 12345678901234\n" $ unlines $ zipWith (++) (map show repeatedSingleDigits) (lines $ displayForPlayer board player)

completeUserTurn :: Player -> Board -> IO (Board, Player)
completeUserTurn player board = do
	(move, updatedBoard, updatedPlayer) <- getMove board player
	if isMoveValid board move then do
		continue <- prompt $ displayToUserForPlayer updatedBoard updatedPlayer ++ "\n" ++ "Is this correct? (y/n): "
		if continue == "y" then
			return (updatedBoard, updatedPlayer)
		else
			completeUserTurn player board
	else do
		putStr "Invalid Move!\n"
		completeUserTurn player board

aiSelectedMove :: Player -> Board -> Move
aiSelectedMove player board = head $ allValidMovesForPlayer player board
		
completeAiTurn :: Player -> Board -> IO (Board, Player)
completeAiTurn player board = return (updatedBoard, updatedPlayer)
	where
		move = aiSelectedMove player board
		updatedBoard = applyMove board move
		updatedPlayer = removePiece player (piece move)

doTurn :: Player -> Board -> IO (Board, Player)
doTurn player board = completeMove player player board

newHuman :: Color -> Player
newHuman color = Player (startingPieces color) color completeUserTurn

newComputer :: Color -> Player
newComputer color = Player (startingPieces color) color completeAiTurn