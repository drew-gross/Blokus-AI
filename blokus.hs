import Control.Applicative

import Data.Maybe
import Data.List
import Data.Function

import Utilities
import Board
import Player
import Color
import Display
import Move
import Chromosome

newComputer :: Color -> Chromosome -> Player
newComputer color chromosome = Player (startingPieces color) color (completeAiTurn chromosome) $ Chromosome.name chromosome 

newHuman :: Color -> Player
newHuman color = Player (startingPieces color) color completeUserTurn "Human Player!"

completeAiTurn :: Chromosome -> Player -> Board -> Player -> IO (Maybe (Board, Player))
completeAiTurn chromosome player board enemy = return $ (,) <$> updatedBoard <*> updatedPlayer
	where
		move = aiSelectedMove chromosome player board enemy
		updatedBoard = applyMove board <$> move
		updatedPlayer = removePiece player <$> piece <$> move

completeUserTurn :: Player -> Board -> Player -> IO (Maybe (Board, Player))
completeUserTurn player board enemy = do
	m <- getMove player board enemy
	if isNothing m then
		return Nothing
	else do
		let (move, updatedBoard, updatedPlayer) = fromJust m
		if isValid board move then do
			continue <- prompt $ displayToUserForPlayer updatedPlayer updatedBoard ++ "\n" ++ "Is this correct? (y/n): "
			if continue == "y" then
				return $ Just (updatedBoard, updatedPlayer)
			else
				completeUserTurn player board enemy
		else do
			putStr "Invalid Move!\n"
			completeUserTurn player board enemy

aiSelectedMove :: Chromosome -> Player -> Board -> Player -> Maybe Move
aiSelectedMove chromosome player board enemy = maybeHead $ reverse $ sortBy (compare `on` fitnessForMove chromosome board enemy) $ validMoves player board

playGame :: (Board, [Player]) -> IO ()
playGame (board, players@(player:enemy:otherPlayers)) = do
	m <- doTurn player board enemy
	if isNothing m then do
		putStr $ (Player.name $ winner players) ++ " beat " ++ (Player.name (head (players \\ [winner players]))) ++ "\n"
	else do
		let (nextBoard, finishedPlayer) = fromJust m
		putStr $ display $ grid nextBoard
		playGame (nextBoard, enemy:otherPlayers ++ [finishedPlayer])

playTournament :: (Board, [[Player]]) -> IO ()
playTournament (board, []) = return ()
playTournament (board, pair:pairs) = do
	playGame (board, pair)
	playTournament (board, pairs)
	return ()

playerPair :: [Chromosome] -> [Player]
playerPair chromosomePair = [newComputer Red $ chromosomePair !! 0, newComputer Blue $ chromosomePair !! 1]

chromosomePairs :: [[Chromosome]]
chromosomePairs = nub $ take 2 <$> permutations chromosomes

main = playTournament (empty2PlayerBoard, playerPair <$> chromosomePairs)