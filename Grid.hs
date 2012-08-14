module Grid (
	Grid(Grid, width),
	containsPoint,
	height,
	itemAt,
	itemIndex,
	changeItemAt,
	changeGridAt
) where
	
import Data.List.Split

import Point
import Utilities

data Grid t = Grid {array :: [t], width :: Int} deriving (Show)

instance ShowToUser t => ShowToUser (Grid t) where
	showToUser = showToUser . rows

height :: Grid t -> Int
height (Grid array width) = fst $ stupidDivision (length array) width 0 False

rows :: Grid t -> [[t]]
rows (Grid array width) = splitEvery width array

containsPoint :: Grid t -> Point -> Bool
containsPoint grid (Point x y) = x >= 0 && x < width grid && y >= 0 && y < height grid

itemIndex :: Grid t -> Point -> Int
itemIndex grid (Point x y) = (y * (width grid)) + x

itemPoint :: Grid t -> Int -> Point
itemPoint (Grid array size) index = Point {x = length array `mod` size, y = length array `div` size}

itemAt :: Grid t -> Point -> t
itemAt grid point
	| not $ containsPoint grid point = error "index out of range"
	| otherwise = array grid !! itemIndex grid point

changeItemAt :: Grid t -> t -> Point -> Grid t
changeItemAt (Grid array width) newItem (Point 0 0) = Grid ([newItem] ++ (tail array)) width
changeItemAt grid newItem point
	| not $ containsPoint grid point = error "index out of range"
	| otherwise = 
		let splitBoard = splitAt (itemIndex grid point) (array grid)
		in Grid ((initOrEmpty $ fst splitBoard) ++ [newItem] ++ snd splitBoard) (width grid)

changeItemsAt :: Grid t -> [t] -> [Point] -> Grid t
changeItemsAt grid indexes points
	| length points == 1 = changeItemAt grid (head indexes) (head points)
	| otherwise = 
		let gridWithFirstItemChanged = changeItemAt grid (head indexes) (head points)
		in changeItemsAt gridWithFirstItemChanged (tail indexes) (tail points)

changeGridAt :: Grid t -> Grid t -> Point -> Grid t
changeGridAt oldGrid newGrid point
	| (length $ array oldGrid) == 1 = changeItemAt oldGrid (head $ array oldGrid) point
	| otherwise = 
		let 
		itemList = (array oldGrid)
		pointList = ([(itemPoint newGrid index) | index <- take (length $ array newGrid) [0..]])
		in changeItemsAt oldGrid itemList pointList