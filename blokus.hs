import Data.Maybe

import Board
import Player
import Color
import Display
import Move

neutralChromosome = [(1.0, squaresUsed),(1.0, launchPointsGained),(1.0, enemyLaunchPointsLost)]
agressiveChromosome = [(1.0, squaresUsed),(1.0, launchPointsGained),(2.0, enemyLaunchPointsLost)]
defensiveChromosome = [(1.0, squaresUsed),(2.0, launchPointsGained),(1.0, enemyLaunchPointsLost)]
kamikaziChromosome = [(0.5, squaresUsed),(0.0, launchPointsGained),(3.0, enemyLaunchPointsLost)]

playGame :: (Board, [Player]) -> IO ()
playGame (board, players@(player:enemy:otherPlayers)) = do
	m <- doTurn player board enemy
	if isNothing m then do
		putStr $ (display $ color $ winner players) ++ " wins!\n"
	else do
		let (nextBoard, finishedPlayer) = fromJust m
		putStr $ display $ grid nextBoard
		playGame (nextBoard, enemy:otherPlayers ++ [finishedPlayer])

main = playGame (empty2PlayerBoard, [newComputer Red agressiveChromosome, newComputer Blue defensiveChromosome])