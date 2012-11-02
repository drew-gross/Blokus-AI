import Debug.Trace

import Color
import Point
import Grid
import Display
import Player
import Board
import Piece
import Move

prevPoint :: Piece -> Point -> Point
prevPoint (Piece grid) (Point 0 y) = Point (width grid - 1) (y - 1)
prevPoint _ point = Point (x point - 1) (y point)

defaultSize = 14
defaultStartPoints = [Point 4 4, Point 9 9]
newBoard = Board (Grid (take (defaultSize * defaultSize) $ repeat Empty) defaultSize) defaultStartPoints
newRedPlayer = (Player 
			(
			 [
			 Piece $ Grid [Red, Red, Red, Red, Red, Empty] 3,
			 Piece $ Grid [Red] 1, 
			 Piece $ Grid [Red, Empty, Red, Red] 2,
			 Piece $ Grid [Red, Red] 1,
			 Piece $ Grid [Red, Red, Red, Red] 2,
			 Piece $ Grid [Red, Red, Red] 1
			 ]
			)
			 Red)
newBluePlayer = (Player 
			(
			 [
			 Piece $ Grid [Blue, Blue, Blue, Blue, Blue, Empty] 3,
			 Piece $ Grid [Blue] 1, 
			 Piece $ Grid [Blue, Empty, Blue, Blue] 2,
			 Piece $ Grid [Blue, Blue] 1,
			 Piece $ Grid [Blue, Blue, Blue, Blue] 2,
			 Piece $ Grid [Blue, Blue, Blue] 1
			 ]
			)
			 Blue)

addPieceToBoardHelper :: Board -> Move -> Point -> Board
addPieceToBoardHelper board move (Point 0 0) = Board (changeItemAt (Board.grid board) (itemAt (Piece.grid $ piece move) origin) (position move)) (startPoints board)
addPieceToBoardHelper board move pieceLocation = 
	let	nextPieceLocation = (prevPoint (piece move) pieceLocation)
		color = (itemAt (Piece.grid (piece move)) pieceLocation)
		updatedGrid = (changeItemAt (Board.grid board) color ((position move) `plus` pieceLocation))
	in addPieceToBoardHelper (Board updatedGrid (startPoints board)) move nextPieceLocation

addPieceToBoard :: Board -> Move -> Board
addPieceToBoard board move = addPieceToBoardHelper board move (maxPoint $ Piece.grid $ piece move)

isMoveInBounds :: Board -> Move -> Bool
isMoveInBounds board (Move piece position)
	| x position < 0 = False
	| y position < 0 = False
	| x (position `plus` maxPoint (Piece.grid piece)) >= width (Board.grid board) = False
	| y (position `plus` maxPoint (Piece.grid piece)) >= height (Board.grid board) = False
	| otherwise = True

isMoveValid :: Board -> Move -> Bool
isMoveValid board move
	| not $ isMoveInBounds board move = False
	| not $ and $ map (isPointValidToColor board (Piece.color $ piece move)) (filledPoints $ piece move) = False
	| or $ map (isPointOpenToColor board (Piece.color $ piece move)) (map (plus $ position move) (filledPoints $ piece move)) = True
	| otherwise = False

isMoveValid2 :: Board -> ((Piece, Int), Point) -> Bool
isMoveValid2 board move = isMoveValid board $ Move (fst $ fst move) (snd move)

allInvalidMovesForPieceRotation :: Board -> (Piece, Int) -> [((Piece, Int), Point)]
allInvalidMovesForPieceRotation board piece = map ((,) piece) (range origin ((maxPoint $ Board.grid board) `minus` (maxPoint $ Piece.grid $ fst piece)))

allValidMovesForPieceRotation :: Board -> (Piece, Int) -> [((Piece, Int), Point)]
allValidMovesForPieceRotation board piece = filter (isMoveValid2 board) (allInvalidMovesForPieceRotation board piece)

allValidMovesForPiece :: Board -> (Piece, Int) -> [((Piece, Int), Point)]
allValidMovesForPiece board piece = concatMap (allValidMovesForPieceRotation board) (zip (rotations $ fst piece) (repeat $ snd piece))

allValidMovesForPlayer :: Board -> Player -> [((Piece, Int), Point)]
allValidMovesForPlayer board player = concatMap (allValidMovesForPiece board) (piecesWithIndices player)

read1IndexdIndex = (flip (-) 1) . read
getPieceFromUserString player string = (pieces player) !! read1IndexdIndex string
moveFromUserInput player pieceIndexStr rotationNumberStr index0point = Move ((rotations $ getPieceFromUserString player pieceIndexStr) !! (read1IndexdIndex rotationNumberStr)) (index0point `minus` (Point 1 1))

completeUserTurn :: (Board, Player) -> IO (Board, Player)
completeUserTurn (board, player) = do
	pieceIndexStr     <- prompt $ displayToUserForPlayer board player ++ "\n" ++ (display player) ++ "\n" ++ "Enter piece number: "
	rotationNumberStr <- prompt $ "Enter rotation number:\n" ++ (displayNumberedList $ rotations $ getPieceFromUserString player pieceIndexStr)
	putStr $ displayToUserForPlayer board player
	index0point <- getPoint
	if isMoveValid board $ moveFromUserInput player pieceIndexStr rotationNumberStr index0point then do
		continue <- prompt $ displayToUserForPlayer (addPieceToBoard board $ moveFromUserInput player pieceIndexStr rotationNumberStr index0point) (removePiece player $ read1IndexdIndex pieceIndexStr) ++ "\n" ++ "Is this correct? (y/n): "
		if continue == "y" then
			return (addPieceToBoard board $ moveFromUserInput player pieceIndexStr rotationNumberStr index0point, removePiece player $ read1IndexdIndex pieceIndexStr)
		else
			completeUserTurn (board, player)
	else do
		putStr "Invalid Move!\n"
		completeUserTurn (board, player)

completeAiTurn :: (Board, Player) -> (Board, Player)
completeAiTurn (board, player) = let
	((piece, pieceIndex), point) = head $ allValidMovesForPlayer board player
	updatedBoard = addPieceToBoard board (Move piece point)
	updatedPlayer = removePiece player pieceIndex
	in (updatedBoard, updatedPlayer)

playGame :: (Board, [Player]) -> IO Board
playGame (board, players) = do
	(nextBoard, nextPlayer) <- completeUserTurn (board, head players)
	let
		(nextBoard2, nextPlayer2) = completeAiTurn (nextBoard, (head $ tail players))
	playGame (nextBoard2, (tail $ tail players) ++ [nextPlayer] ++ [nextPlayer2])

main = playGame (newBoard, [newRedPlayer, newBluePlayer])