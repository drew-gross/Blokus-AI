import Board
import Player
import Color

playGame :: (Board, [Player]) -> IO Board
playGame (board, nextHuman:nextAI:otherPlayers) = do
	(nextBoard, nextPlayer) <- doTurn nextHuman (board, nextHuman)
	(nextBoard2, nextPlayer2) <- doTurn nextAI  (nextBoard, nextAI)
	playGame (nextBoard2, (otherPlayers) ++ [nextPlayer] ++ [nextPlayer2])

main = playGame (empty2PlayerBoard, [newHuman Red, newComputer Blue])