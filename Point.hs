module Point
(
	Point(Point, x, y),
	origin,

	leftPoint,
	rightPoint,
	upPoint,
	downPoint,

	ulPoint,
	urPoint,
	dlPoint,
	drPoint,

	flipAboutYequalsX,

	plus,
	minus,
	rubikDistanceToIntersection,

	range,
	transposeRange,

	getPoint,
	read1IndexedPoint
) where

import Utilities
import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

data Point = Point {x :: Int, y :: Int} deriving (Show, Eq)

origin = Point 0 0

leftPoint :: Point -> Point
leftPoint = plus $ Point (-1) 0

rightPoint :: Point -> Point
rightPoint = plus $ Point 1 0

upPoint :: Point -> Point
upPoint = plus $ Point 0 (-1)

downPoint :: Point -> Point
downPoint = plus $ Point 0 1

ulPoint = upPoint . leftPoint
urPoint = upPoint . rightPoint
dlPoint = downPoint . leftPoint
drPoint = downPoint . rightPoint

flipAboutYequalsX :: Point -> Point
flipAboutYequalsX (Point x y) = Point y x

plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

rubikDistanceToIntersection :: Point -> Point -> Int
rubikDistanceToIntersection (Point pointX pointY) (Point intersectionX intersectionY) = hDist + vDist
	where
		hDist = if pointX < intersectionX then
					intersectionX - pointX - 1
				else if pointX > intersectionX then
					pointX - intersectionX
				else 
					0
		vDist = if pointY < intersectionY then
					intersectionY - pointY - 1
				else if pointY > intersectionY then
					pointY - intersectionY
				else
					0

range :: Point -> Point -> [Point]
range (Point startX startY) (Point endX endY) = [Point x y | y <- [startY..endY], x <- [startX..endX]]

transposeRange :: Point -> Point -> [Point]
transposeRange (Point startX startY) (Point endX endY) = [Point x y | x <- [startX..endX], y <- [startY..endY]]

read1IndexedPoint = (flip minus $ Point 1 1)

getPoint :: MaybeT IO Point
getPoint = Point <$> (MaybeT $ maybeRead <$> prompt "Enter x: ") <*> (MaybeT $ maybeRead <$> prompt "Enter y: ")