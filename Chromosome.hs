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

data Gene = Gene {weight :: Double, function :: Board -> Player -> Move -> Double}

valueForMove :: Board -> Player -> Move -> Gene -> Double
valueForMove board enemy move (Gene weight function) = function board enemy move * weight

squaresUsed :: Fractional a => Board -> Player -> Move -> a
squaresUsed _ _ (Move piece _) = fromIntegral $ length $ filledPoints piece

launchPointsGained :: Fractional a => Board -> Player -> Move -> a
launchPointsGained board _ move@(Move piece _) = fromIntegral $ numOfLaunchPointsForColor (applyMove board move) color - numOfLaunchPointsForColor board color
	where
		color = Piece.color piece

enemyLaunchPointsLost :: Fractional a => Board -> Player -> Move -> a
enemyLaunchPointsLost board enemy move@(Move piece _) = fromIntegral $ numOfLaunchPointsForColor board enemyColor - numOfLaunchPointsForColor (applyMove board move) enemyColor
	where
		enemyColor = Player.color enemy

rubikDistanceToCenter :: Fractional a => Board -> Player -> Move -> a
rubikDistanceToCenter board _ move@(Move piece position) = fromIntegral $ foldr (min) 100 rubikDistances --100 chosen arbitrarily, its larger than any board out there
	where
		centerInter = centerIntersection $ Board.grid board
		points = filledPointsOnBoard move
		rubikDistances :: [Int]
		rubikDistances = (flip rubikDistanceToIntersection) centerInter <$> points

data Chromosome = Chromosome {genes :: [Gene], name :: String}

instance Eq Chromosome where
	(==) left right = name left == name right
	(/=) left right = not $ left == right

fitnessForMove :: Chromosome -> Board -> Player -> Move -> Double
fitnessForMove (Chromosome genes _) board enemy move = sum $ valueForMove board enemy move <$> genes

chromosomePairsToCheck = nub $ take 2 <$> permutations chromosomes

chromosomes = [
				Chromosome [Gene 1.0 squaresUsed, Gene 1.0 launchPointsGained, Gene 1.0 enemyLaunchPointsLost, Gene (-1.0) rubikDistanceToCenter] "neutral",
				Chromosome [Gene 1.0 squaresUsed, Gene 1.0 launchPointsGained, Gene 2.0 enemyLaunchPointsLost, Gene (-1.0) rubikDistanceToCenter] "agressive",
				Chromosome [Gene 1.0 squaresUsed, Gene 2.0 launchPointsGained, Gene 1.0 enemyLaunchPointsLost, Gene (-1.0) rubikDistanceToCenter] "defensive",
				Chromosome [Gene 0.5 squaresUsed, Gene 0.0 launchPointsGained, Gene 3.0 enemyLaunchPointsLost, Gene (-1.0) rubikDistanceToCenter] "kamikazi"
			  ]