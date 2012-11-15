module Grid (
	Grid(width, height),
	makeEmptyGrid,
	makeFilledGrid,

	containsPoint,
	maxPoint,
	allPoints,

	unsafeItemAt,
	itemAt,
	itemIndex,
	changeItemAt,
	changeGridAt,

	color,

	flipAboutVertical,

	rotate90,
	rotate180,
	rotate270,
) where
	
import Data.List.Split
import Data.Maybe

import Point
import Display
import Utilities
import Color

data Grid t = Grid {array :: [t], width :: Int, height :: Int} deriving (Show, Eq)

instance Display t => Display (Grid t) where
	display = display . rows

makeEmptyGrid :: Int -> Int -> t -> Grid t
makeEmptyGrid width height defaultCell = Grid (take arraySize $ repeat defaultCell) width height
	where
		arraySize = width * height

makeFilledGrid :: [t] -> Int -> Grid t
makeFilledGrid array width = Grid array width $ (length array) `div` width

rows :: Grid t -> [[t]]
rows (Grid array width _) = chunksOf width array

maxPoint :: Grid t -> Point
maxPoint grid = Point ((width grid) - 1) ((height grid) - 1)

allPoints :: Grid t -> [Point]
allPoints grid = range origin (maxPoint grid)

containsPoint :: Grid t -> Point -> Bool
containsPoint grid (Point x y) = x >= 0 && x < width grid && y >= 0 && y < height grid

itemIndex :: Grid t -> Point -> Int
itemIndex grid (Point x y) = (y * (width grid)) + x

itemPoint :: Grid t -> Int -> Point
itemPoint (Grid array width _) index = Point {x = length array `mod` width, y = length array `div` width}

--For use when you know you aren't going to access out of bounds
unsafeItemAt :: Grid t -> Point -> t
unsafeItemAt grid point 
	| isNothing item = error "unsafeItemAt: accessing item out of grid bounds"
	| otherwise = fromJust item 
	where item = itemAt grid point

itemAt :: Grid t -> Point -> Maybe t
itemAt grid@(Grid array _ _) point
	| not $ containsPoint grid point = Nothing
	| otherwise = maybeIndex array $ itemIndex grid point

changeItemAt :: Grid t -> t -> Point -> Grid t
changeItemAt grid@(Grid array width height) newItem point = Grid [if index == newItemIndex then newItem else item | (index, item) <- indexItemPairs] width height
	where
		newItemIndex = itemIndex grid point
		indexItemPairs = zip [0..] array

changeItemsAt :: Grid t -> [t] -> [Point] -> Grid t
changeItemsAt grid (index:indexes) (point:points)
	| length points == 1 = gridWithFirstItemChanged
	| otherwise = changeItemsAt gridWithFirstItemChanged indexes points
	where
		gridWithFirstItemChanged = changeItemAt grid index point

changeGridAt :: Grid t -> Grid t -> Point -> Grid t
changeGridAt oldGrid@(Grid oldArray@(oldHead:oldTail) _ _) newGrid@(Grid newArray _ _) point
	| length oldArray == 1 = changeItemAt oldGrid oldHead point
	| otherwise = changeItemsAt oldGrid oldArray pointList
	where
		pointList = [itemPoint newGrid index | index <- take (length newArray) [0..]]

flipAboutVertical :: Grid t -> Grid t
flipAboutVertical grid@(Grid _ width height) = Grid newArray width height
	where
		newArray = [unsafeItemAt grid $ Point (width - x - 1) y | Point x y <- range origin $ maxPoint grid]

color :: Grid Color -> Color
color (Grid array _ _) = head $ filter (/= Empty) array

transpose :: Grid t -> Grid t
transpose grid@(Grid _ width height) = Grid newArray height width
	where
		newArray = [unsafeItemAt grid point | point <- transposeRange origin $ maxPoint grid]

rotate90 = flipAboutVertical . transpose

rotate180 :: Grid t -> Grid t
rotate180 (Grid array width height) = Grid (reverse array) width height

rotate270  = rotate90 . rotate180
