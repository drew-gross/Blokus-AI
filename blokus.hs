import Board
import Player
import Color

playGame :: (Board, [Player]) -> IO Board
playGame (board, currentPlayer:otherPlayers) = do
	(nextBoard, finishedPlayer) <- doTurn currentPlayer (board, currentPlayer)
	playGame (nextBoard, otherPlayers ++ [finishedPlayer])

main = playGame (empty2PlayerBoard, [newHuman Red, newComputer Blue])