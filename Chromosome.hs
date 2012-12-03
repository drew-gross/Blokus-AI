module Chromosome (
	Chromosome(Chromosome, name),
	Gene(Gene),
	chromosomes,

	fitnessForMove,

	squaresUsed,
	launchPointsGained,
	enemyLaunchPointsLost,
	rubikDistanceToCenter
) where

import Control.Applicative

import Data.List

import Utilities
import Player hiding (name)
import Move
import Piece
import Board
import Grid
import Point

type FitnessFunction = Board -> Player -> Move -> Double

data Gene = Gene {weight :: Double, function :: FitnessFunction}

makeGenes :: [Double] -> [FitnessFunction] -> [Gene]
makeGenes weights functions = Gene <$> weights <*> functions

valueForMove :: Board -> Player -> Move -> Gene -> Double
valueForMove board enemy move (Gene weight function) = function board enemy move * weight

squaresUsed :: Fractional a => Board -> Player -> Move -> a
squaresUsed _ _ (Move piece _) = fromIntegral $ length $ filledPoints piece

launchPointsGained :: Fractional a => Board -> Player -> Move -> a
launchPointsGained board _ move@(Move piece _) = fromIntegral $ numOfLaunchPointsForColor (applyMove board move) color - numOfLaunchPointsForColor board color
	where
		color = Piece.color piece

enemyLaunchPointsLost :: Fractional a => Board -> Player -> Move -> a
enemyLaunchPointsLost board enemy move@(Move piece _) = fromIntegral $ enemyLaunchPointsNow - numOfLaunchPointsForColor (applyMove board move) enemyColor
	where
		enemyColor = Player.color enemy
		enemyLaunchPointsNow = numOfLaunchPointsForColor board enemyColor

rubikDistanceToCenter :: Fractional a => Board -> Player -> Move -> a
rubikDistanceToCenter board _ move@(Move piece position) = fromIntegral $ foldr (min) 100 rubikDistances --TODO: 100 chosen arbitrarily, its larger than any board out there
	where
		centerInter = centerIntersection $ Board.grid board
		points = filledPointsOnBoard move
		rubikDistances :: [Int]
		rubikDistances = (flip rubikDistanceToIntersection) centerInter <$> points

data Chromosome = Chromosome {genes :: [Gene], name :: String}

instance Show Chromosome where
	show = name

instance Eq Chromosome where
	(==) left right = name left == name right
	(/=) left right = not $ left == right

makeChromosomes :: [[Double]] -> [FitnessFunction] -> [Chromosome] 
makeChromosomes [] _ = []
makeChromosomes [weights] functions = [Chromosome (makeGenes weights functions) $ show weights]
makeChromosomes (firstWeights:otherWeights) functions = makeChromosomes [firstWeights] functions ++ makeChromosomes otherWeights functions

fitnessForMove :: Chromosome -> Board -> Player -> Move -> Double
fitnessForMove (Chromosome genes _) board enemy move = sum $ valueForMove board enemy move <$> genes

functions = [squaresUsed, launchPointsGained, enemyLaunchPointsLost, rubikDistanceToCenter]
weights = combinationsOfLength 4 [1.0, (-1.0)]
chromosomes = reverse $ makeChromosomes weights functions