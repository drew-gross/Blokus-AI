module Point
(
	Point(Point, x, y),
	leftPoint,
	rightPoint,
	upPoint,
	downPoint,

	ulPoint,
	urPoint,
	dlPoint,
	drPoint,
	plus,

	range
) where

data Point = Point {x :: Int, y :: Int} deriving (Show)

plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

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

range :: Point -> Point -> [Point]
range (Point startX startY) (Point endX endY) = [Point x y | y <- [startX..endX], x <- [startY..endY]]