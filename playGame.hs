import Game
import Board
import Chromosome
import Color

main = playGame (empty2PlayerBoard, [newComputer yellow $ head chromosomes, newComputer red $ last chromosomes]) False