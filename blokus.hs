import Data.Maybe

import Board
import Player
import Color
import Display
import Move

playGame :: (Board, [Player]) -> IO ()
playGame (board, players@(player:otherPlayers)) = do
	m <- doTurn player board
	if isNothing m then do
		putStr $ (display $ color $ winner players) ++ " wins!\n"
	else do
		let (nextBoard, finishedPlayer) = fromJust m
		putStr $ display $ grid nextBoard
		playGame (nextBoard, otherPlayers ++ [finishedPlayer])

main = playGame (empty2PlayerBoard, [newComputer Red, newComputer Blue])