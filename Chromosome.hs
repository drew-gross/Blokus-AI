module Chromosome (
	Chromosome(name),
	chromosomePairsToCheck,
	chromosomes,

	fitnessForMove
) where

import Control.Applicative

import Data.List

import Player hiding (name)
import Move
import Piece
import Board
import Grid
import Point

data Gene = Gene {weight :: Double, function :: Move -> Player -> Double}

valueForMove :: Gene -> Player -> Move -> Double
valueForMove (Gene weight function) enemy move = function move enemy * weight

squaresUsed :: Fractional a => Move -> Player -> a
squaresUsed (Move piece _ _) _ = fromIntegral $ length $ filledPoints piece

launchPointsGained :: Fractional a => Move -> Player -> a
launchPointsGained move@(Move piece board _) _ = fromIntegral $ numOfLaunchPointsForColor (apply move) color - numOfLaunchPointsForColor board color
	where
		color = Piece.color piece

enemyLaunchPointsLost :: Fractional a => Move -> Player -> a
enemyLaunchPointsLost move@(Move piece board _) enemy = fromIntegral $ numOfLaunchPointsForColor board enemyColor - numOfLaunchPointsForColor (apply move) enemyColor
	where
		enemyColor = Player.color enemy

rubikDistanceToCenter :: Fractional a => Move -> Player -> a
rubikDistanceToCenter move@(Move piece board position) _ = fromIntegral $ foldr (min) 100 rubikDistances --100 chosen arbitrarily, its larger than any board out there
	where
		centerInter = centerIntersection $ Board.grid board
		points = filledPointsOnBoard move
		rubikDistances :: [Int]
		rubikDistances = (flip rubikDistanceToIntersection) centerInter <$> points

data Chromosome = Chromosome {genes :: [Gene], name :: String}

instance Eq Chromosome where
	(==) left right = name left == name right
	(/=) left right = not $ left == right

fitnessForMove :: Chromosome -> Player -> Move -> Double
fitnessForMove (Chromosome genes _) enemy move = sum weightedValues
	where
		valueForMove' :: Player -> Move -> Gene -> Double
		valueForMove' enemy move gene = valueForMove gene enemy move
		weightedValues = valueForMove' enemy move <$> genes

chromosomePairsToCheck = nub $ take 2 <$> permutations chromosomes

chromosomes = [
				Chromosome [Gene 1.0 squaresUsed, Gene 1.0 launchPointsGained, Gene 1.0 enemyLaunchPointsLost, Gene (-1.0) rubikDistanceToCenter] "neutral",
				Chromosome [Gene 1.0 squaresUsed, Gene 1.0 launchPointsGained, Gene 2.0 enemyLaunchPointsLost, Gene (-1.0) rubikDistanceToCenter] "agressive",
				Chromosome [Gene 1.0 squaresUsed, Gene 2.0 launchPointsGained, Gene 1.0 enemyLaunchPointsLost, Gene (-1.0) rubikDistanceToCenter] "defensive",
				Chromosome [Gene 0.5 squaresUsed, Gene 0.0 launchPointsGained, Gene 3.0 enemyLaunchPointsLost, Gene (-1.0) rubikDistanceToCenter] "kamikazi"
			  ]