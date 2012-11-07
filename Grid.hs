module Grid (
	Grid(Grid, width),
	containsPoint,
	maxPoint,
	allPoints,

	height,
	itemAt,
	safeItemAt,
	itemIndex,
	changeItemAt,
	changeGridAt,

	flipAboutVertical,

	rotate90,
	rotate180,
	rotate270,
) where
	
import Data.List.Split

import Point
import Display

data Grid t = Grid {array :: [t], width :: Int} deriving (Show, Eq)

instance Display t => Display (Grid t) where
	display = display . rows

height :: Grid t -> Int
height (Grid array width) = (length array) `div` width

rows :: Grid t -> [[t]]
rows (Grid array width) = splitEvery width array

maxPoint :: Grid t -> Point
maxPoint grid = Point ((width grid) - 1) ((height grid) - 1)

allPoints :: Grid t -> [Point]
allPoints grid = range origin (maxPoint grid)

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

safeItemAt :: Grid t -> Point -> [t]
safeItemAt grid point
	| not $ containsPoint grid point = []
	| otherwise = [itemAt grid point]

changeItemAt :: Grid t -> t -> Point -> Grid t
changeItemAt grid newItem point = 
	let
		newItemIndex = itemIndex grid point
		indexItemPairs = zip [0..] $ array grid
	in Grid [if index == newItemIndex then newItem else item | (index, item) <- indexItemPairs] $ width grid

changeItemsAt :: Grid t -> [t] -> [Point] -> Grid t
changeItemsAt grid (index:indexes) (point:points)
	| length points == 1 = gridWithFirstItemChanged
	| otherwise = changeItemsAt gridWithFirstItemChanged indexes points
	where
		gridWithFirstItemChanged = changeItemAt grid index point

changeGridAt :: Grid t -> Grid t -> Point -> Grid t
changeGridAt oldGrid newGrid point
	| length itemList == 1 = changeItemAt oldGrid (head itemList) point
	| otherwise = changeItemsAt oldGrid itemList pointList
	where
		itemList = array oldGrid
		pointList = [itemPoint newGrid index | index <- take (length $ array newGrid) [0..]]

flipAboutVertical :: Grid t -> Grid t
flipAboutVertical grid = let
	newArray = [itemAt grid $ Point ((width grid) - x - 1) y | Point x y <- range origin $ maxPoint grid]
	newWidth = width grid
	newGrid = Grid newArray newWidth
	in newGrid

transpose :: Grid t -> Grid t
transpose grid = let
	newArray = [itemAt grid point | point <- transposeRange origin $ maxPoint grid]
	newWidth = height grid
	newGrid = Grid newArray newWidth
	in newGrid

rotate90 = flipAboutVertical . transpose

rotate180 :: Grid t -> Grid t
rotate180 grid = Grid (reverse $ array grid) (width grid)

rotate270  = rotate90 . rotate180
