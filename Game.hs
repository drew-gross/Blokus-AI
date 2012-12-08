module Game (
	playGame,
	playTournament
) where

main = putStrLn "Hello World"

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

import Data.Maybe
import Data.List
import Data.Function

import System.Posix.IO

import Utilities
import Board
import Player
import Color
import Display
import Move
import Chromosome

playGame :: (Board, [Player]) -> Bool -> IO [Player]
playGame (board, players@(player:enemy:otherPlayers)) isGameOver
	| length players /= 2 = error "Only 2 player games are supported for now"
	| otherwise = do
		m <- doTurn player board enemy
		if isNothing m && isGameOver then do
			return $! [winingPlayer, losingPlayer]
		else if isNothing m then 
			playGame (board, enemy:otherPlayers ++ [player]) True --current player can't move, put them on the back of the stack and let next player go
		else do
			let (nextBoard, finishedPlayer) = fromJust m
			putStr $ display $ grid nextBoard
			playGame (nextBoard, enemy:otherPlayers ++ [finishedPlayer]) False
	where
		winingPlayer = winner players
		losingPlayer = head (players \\ [winingPlayer])

playTournament :: (Board, [[Player]]) -> IO [[Player]]
playTournament (board, []) = return []
playTournament (board, pair:pairs) = do
	(:) <$> result <*> otherResults
	where
		result = playGame (board, pair) False
		otherResults = playTournament (board, pairs)

playerPair :: [Chromosome] -> [Player]
playerPair (first:second:[]) = [newComputer red first, newComputer blue second]

chromosomePairs :: [[Chromosome]]
chromosomePairs = nub $ take 2 <$> permutations chromosomes

main = do
	results <- playTournament (empty2PlayerBoard, playerPair <$> chromosomePairs)
	writeFile "results/tournament.txt" $ concatMap playersToString results
	where
		playersToString :: [Player] -> String
		playersToString (winner:loser:[]) = show winner ++ " beat " ++ show loser ++ "\n"

--main = playGame (empty2PlayerBoard, [newHuman Yellow "Drew", newComputer Red (last chromosomes)]) False