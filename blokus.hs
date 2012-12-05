import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

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

newHuman :: Color -> String -> Player
newHuman color name = Player (startingPieces color) color completeUserTurn name

completeAiTurn :: Chromosome -> Player -> Board -> Player -> MaybeT IO (Board, Player)
completeAiTurn chromosome player board enemy = (,) <$> updatedBoard <*> updatedPlayer
	where
		move = MaybeT $ return $ aiSelectedMove chromosome player board enemy
		updatedBoard = applyMove board <$> move
		updatedPlayer = removePiece player <$> piece <$> move

completeUserTurn :: Player -> Board -> Player -> MaybeT IO (Board, Player)
completeUserTurn player board enemy = do
	(move, updatedBoard, updatedPlayer) <- ioMove
	if isValid board move then do
		continue <- lift $ prompt $ displayToUserForPlayer updatedPlayer updatedBoard ++ "\n" ++ "Is this correct? (y/n): "
		if continue == "y" then
			return (updatedBoard, updatedPlayer)
		else
			completeUserTurn player board enemy
	else do
		lift $ putStr "Invalid Move!\n"
		completeUserTurn player board enemy
	where
		ioMove = getMove player board enemy

aiSelectedMove :: Chromosome -> Player -> Board -> Player -> Maybe Move
aiSelectedMove chromosome player board enemy = maybeHead $ reverse $ sortBy (compare `on` fitnessForMove chromosome board enemy) $ validMoves player board

playGame :: (Board, [Player]) -> Bool -> IO ()
playGame (board, players@(player:enemy:otherPlayers)) isGameOver
	| length players /= 2 = error "Only 2 player games are supported for now"
	| otherwise = do
		m <- runMaybeT $ doTurn player board enemy
		if isNothing m && isGameOver then do
			putStr $ winnerString ++ " beat " ++ loserString ++ "\n"
		else if isNothing m then 
			playGame (board, enemy:otherPlayers ++ [player]) True --current player can't move, put them on the back of the stack and let next player go
		else do
			let (nextBoard, finishedPlayer) = fromJust m
			putStr $ display $ grid nextBoard
			playGame (nextBoard, enemy:otherPlayers ++ [finishedPlayer]) False
	where
		winingPlayer = winner players
		losingPlayer = head (players \\ [winingPlayer])
		winnerString = displayColored (Player.color winingPlayer) (Player.name winingPlayer)
		loserString = displayColored (Player.color losingPlayer) (Player.name losingPlayer)

playTournament :: (Board, [[Player]]) -> IO ()
playTournament (board, []) = return ()
playTournament (board, pair:pairs) = do
	playGame (board, pair) False
	playTournament (board, pairs)

playerPair :: [Chromosome] -> [Player]
playerPair chromosomePair = [newComputer Red $ chromosomePair !! 0, newComputer Blue $ chromosomePair !! 1]

chromosomePairs :: [[Chromosome]]
chromosomePairs = nub $ take 2 <$> permutations chromosomes

main = playTournament (empty2PlayerBoard, playerPair <$> chromosomePairs)
--main = playGame (empty2PlayerBoard, [newHuman Yellow "Drew", newComputer Red (last chromosomes)]) False