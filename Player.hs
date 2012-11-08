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
allInvalidMovesForPieceRotation board piece = let
		maxPlacementPoint = ((maxPoint $ Board.grid board) `minus` (maxPoint $ Piece.grid piece))
	in map (Move piece) (range origin maxPlacementPoint)

allValidMovesForPieceRotation :: Board -> Piece -> [Move]
allValidMovesForPieceRotation board piece = filter (isMoveValid board) (allInvalidMovesForPieceRotation board piece)

allValidMovesForPiece :: Board -> Piece -> [Move]
allValidMovesForPiece board piece = concatMap (allValidMovesForPieceRotation board) (rotations piece)

allValidMovesForPlayer :: Board -> Player -> [Move]
allValidMovesForPlayer board player = concatMap (allValidMovesForPiece board) (pieces player)

read1IndexdIndex = (flip (-) 1) . read
read1IndexdPoint = (flip minus $ Point 1 1)

getMove :: Board -> Player -> IO (Move, Board, Player)
getMove board player = do
	unrotatedPiece <- fmap ((!!) (pieces player)) $ fmap read1IndexdIndex $ prompt $ displayToUserForPlayer board player ++ "\n" ++ (display player) ++ "\n" ++ "Enter piece number: "
	rotationNumber <- fmap read1IndexdIndex $ prompt $ (displayNumberedList $ rotations unrotatedPiece) ++ "\n" ++ "Enter rotation number:"
	putStr $ displayToUserForPlayer board player
	let	piece = rotations unrotatedPiece !! rotationNumber
	move <- Move <$> (return piece) <*> (fmap read1IndexdPoint getPoint)
	return (move, applyMove board move, removePiece player piece)

displayForPlayer :: Board -> Player -> String
displayForPlayer board player = let
	chars = map (displayChar board (Player.color player)) (range origin (maxPoint $ Board.grid board))
	splitChars = chunksOf (width $ Board.grid board) chars 
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
		
completeAiTurn :: Player -> Board -> IO (Board, Player)
completeAiTurn player board = let
	move = head $ allValidMovesForPlayer board player
	updatedBoard = applyMove board move
	updatedPlayer = removePiece player (piece move)
	in return (updatedBoard, updatedPlayer)

doTurn :: Player -> Board -> IO (Board, Player)
doTurn player board = completeMove player player board

newHuman :: Color -> Player
newHuman color = Player (startingPieces color) color completeUserTurn

newComputer :: Color -> Player
newComputer color = Player (startingPieces color) color completeAiTurn