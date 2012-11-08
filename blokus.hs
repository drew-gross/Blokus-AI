import Board
import Player
import Color

playGame :: (Board, [Player]) -> IO Board
playGame (board, player:otherPlayers) = do
	(nextBoard, finishedPlayer) <- doTurn player board
	playGame (nextBoard, otherPlayers ++ [finishedPlayer])

main = playGame (empty2PlayerBoard, [newHuman Red, newComputer Blue])