module Player(
	Player(Player, color, pieces),
	doTurn,
	removePiece,
	newHuman,
	newComputer,
	winner
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
import Move
import Point

data Player = Player {pieces :: [Piece], color :: Color, completeMove :: Player -> Board -> IO (Maybe (Board, Player))}

instance Display Player where
	display player = let
		pieceAnnotations = ["Piece " ++ show num ++ ":\n" | num <- [1..]]
		pieceStrings = map display $ pieces player
		in concat $ zipWith (++) pieceAnnotations pieceStrings

newHuman :: Color -> Player
newHuman color = Player (startingPieces color) color completeUserTurn

newComputer :: Color -> Player
newComputer color = Player (startingPieces color) color completeAiTurn

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

allValidMovesForPlayer :: Player -> Board -> [Move]
allValidMovesForPlayer (Player pieces _ _) board = concatMap (validMovesForPiece board) pieces

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

getMove :: Player -> Board -> IO (Maybe (Move, Board, Player))
getMove player board = do
	piece <- getRotatedPiece player board
	move <- Move <$> (return piece) <*> (return board) <*> (fmap read1IndexedPoint getPoint)
	return $ Just (move, apply move, removePiece player piece)

displayForPlayer :: Player -> Board -> String
displayForPlayer (Player _ color _) board@(Board grid sp) = unlines $ concat splitChars
	where
		strings = map (displayString board color) (range origin $ maxPoint grid)
		splitChars = chunksOf (width grid) strings 

displayToUserForPlayer :: Player -> Board -> String
displayToUserForPlayer player board = (++) " 12345678901234\n" $ unlines $ zipWith (++) (map show repeatedSingleDigits) (lines $ displayForPlayer player board)

completeUserTurn :: Player -> Board -> IO (Maybe (Board, Player))
completeUserTurn player board = do
	m <- getMove player board
	if isNothing m then
		return Nothing
	else do
		let (move, updatedBoard, updatedPlayer) = fromJust m
		if isValid move then do
			continue <- prompt $ displayToUserForPlayer updatedPlayer updatedBoard ++ "\n" ++ "Is this correct? (y/n): "
			if continue == "y" then
				return $ Just (updatedBoard, updatedPlayer)
			else
				completeUserTurn player board
		else do
			putStr "Invalid Move!\n"
			completeUserTurn player board

aiSelectedMove :: Player -> Board -> Maybe Move
aiSelectedMove player board = maybeHead $ reverse $ sortBy (compare `on` squaresUsed) $ allValidMovesForPlayer player board

completeAiTurn :: Player -> Board -> IO (Maybe (Board, Player))
completeAiTurn player board = return $ (,) <$> updatedBoard <*> updatedPlayer
	where
		move = aiSelectedMove player board
		updatedBoard = fmap apply move
		updatedPlayer = fmap (removePiece player) $ fmap piece move

doTurn :: Player -> Board -> IO (Maybe (Board, Player))
doTurn player board = completeMove player player board

squaresRemaining :: Player -> Int
squaresRemaining (Player pieces _ _)= sum $ map filledPointsCount pieces

winner :: [Player] -> Player
winner players = head $ sortBy (compare `on` squaresRemaining) players