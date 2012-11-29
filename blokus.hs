import Control.Applicative

import Data.Maybe
import Data.List

import Utilities
import Board
import Player
import Color
import Display
import Move
import Chromosome

newComputer :: (Fractional a, Ord a) => Color -> Chromosome -> Player
newComputer color chromosome = Player (startingPieces color) color $ completeAiTurn chromosome

newHuman :: Color -> Player
newHuman color = Player (startingPieces color) color completeUserTurn

completeAiTurn :: (Ord a, Fractional a) => [(a, Move -> Player -> a)] -> Player -> Board -> Player -> IO (Maybe (Board, Player))
completeAiTurn chromosome player board enemy = return $ (,) <$> updatedBoard <*> updatedPlayer
	where
		move = aiSelectedMove chromosome player board enemy
		updatedBoard = apply <$> move
		updatedPlayer = removePiece player <$> piece <$> move

completeUserTurn :: Player -> Board -> Player -> IO (Maybe (Board, Player))
completeUserTurn player board enemy = do
	m <- getMove player board enemy
	if isNothing m then
		return Nothing
	else do
		let (move, updatedBoard, updatedPlayer) = fromJust m
		if isValid move then do
			continue <- prompt $ displayToUserForPlayer updatedPlayer updatedBoard ++ "\n" ++ "Is this correct? (y/n): "
			if continue == "y" then
				return $ Just (updatedBoard, updatedPlayer)
			else
				completeUserTurn player board enemy
		else do
			putStr "Invalid Move!\n"
			completeUserTurn player board enemy

aiSelectedMove :: (Ord a, Fractional a) => [(a, Move -> Player -> a)] -> Player -> Board -> Player -> Maybe Move
aiSelectedMove chromosome player board enemy = maybeHead $ reverse $ sortBy (compare `on` fitness' enemy chromosome) $ validMovesForPlayer player board
	where
		fitness' :: Fractional a => Player -> [(a, Move -> Player -> a)] -> Move -> a
		fitness' player chromosome move = fitness move chromosome player

neutralChromosome = [(1.0, squaresUsed),(1.0, launchPointsGained),(1.0, enemyLaunchPointsLost), (-1.0, rubikDistanceToCenter)]
agressiveChromosome = [(1.0, squaresUsed),(1.0, launchPointsGained),(2.0, enemyLaunchPointsLost), (-1.0, rubikDistanceToCenter)]
defensiveChromosome = [(1.0, squaresUsed),(2.0, launchPointsGained),(1.0, enemyLaunchPointsLost), (-1.0, rubikDistanceToCenter)]
kamikaziChromosome = [(0.5, squaresUsed),(0.0, launchPointsGained),(3.0, enemyLaunchPointsLost), (-1.0, rubikDistanceToCenter)]

playGame :: (Board, [Player]) -> IO ()
playGame (board, players@(player:enemy:otherPlayers)) = do
	m <- doTurn player board enemy
	if isNothing m then do
		putStr $ (display $ color $ winner players) ++ " wins!\n"
	else do
		let (nextBoard, finishedPlayer) = fromJust m
		putStr $ display $ grid nextBoard
		playGame (nextBoard, enemy:otherPlayers ++ [finishedPlayer])

chromosomes = [neutralChromosome, agressiveChromosome, defensiveChromosome, kamikaziChromosome]

main = playGame (empty2PlayerBoard, [newComputer Red kamikaziChromosome, newComputer Blue defensiveChromosome])