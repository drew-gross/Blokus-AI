import Data.List

import Control.Applicative

import Game
import Player
import Board
import Chromosome
import Color

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