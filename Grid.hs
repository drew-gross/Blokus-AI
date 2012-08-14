module Grid (
	Grid(Grid, width),
	containsPoint,
	height,
	itemAt,
	itemIndex,
	changeItemAt
) where
	
import Data.List.Split

import Point
import Utilities

data Grid t = Grid {array :: [t], width :: Int} deriving (Show)

instance ShowToUser t => ShowToUser (Grid t) where
	showToUser (Grid array width) = showToUser $ splitEvery width array

height :: Grid t -> Int
height (Grid array width) = fst $ stupidDivision (length array) width 0 False

containsPoint :: Grid t -> Point -> Bool
containsPoint grid (Point x y) = x >= 0 && x < width grid && y >= 0 && y < height grid

itemIndex :: Grid t -> Point -> Int
itemIndex grid point = ((y point) * (width grid)) + (x point)

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