module Grid (
	Grid(width, height),
	makeEmptyGrid,
	makeFilledGridWithList,

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
import Data.Vector
import Data.Maybe

import Point
import Display
import Utilities
import Color

data Grid t = Grid {array :: Vector t, width :: Int, height :: Int} deriving (Show, Eq)

instance Display t => Display (Grid t) where
	display = display . rows

makeEmptyGrid :: Int -> Int -> t -> Grid t
makeEmptyGrid width height defaultCell = Grid (fromList $ Prelude.replicate arraySize defaultCell) width height
	where
		arraySize = width * height

makeFilledGridWithList :: [t] -> Int -> Grid t
makeFilledGridWithList array width 
	| Prelude.null array = error "can't use an empty list!"
	| arrayLength `mod` width /= 0 = error "array length isn't a muliple of the width!"
	| otherwise = Grid (fromList array) width $ arrayLength `div` width
	where
		arrayLength = Prelude.length array

rows :: Grid t -> [[t]]
rows (Grid array width _) = chunksOf width $ toList array

maxPoint :: Grid t -> Point
maxPoint grid = Point ((width grid) - 1) ((height grid) - 1)

allPoints :: Grid t -> [Point]
allPoints grid = range origin (maxPoint grid)

containsPoint :: Grid t -> Point -> Bool
containsPoint grid (Point x y)
	| x < 0 = False
	| y < 0 = False
	| x >= width grid = False
	| y >= height grid = False
	| otherwise = True

itemIndex :: Grid t -> Point -> Int
itemIndex grid (Point x y) = (y * (width grid)) + x

itemPoint :: Grid t -> Int -> Point
itemPoint (Grid array width _) index = Point {x = Data.Vector.length array `mod` width, y = Data.Vector.length array `div` width}

--For use when you know you aren't going to access out of bounds
unsafeItemAt :: Grid t -> Point -> t
unsafeItemAt grid point 
	| isNothing item = error "unsafeItemAt: accessing item out of grid bounds"
	| otherwise = fromJust item 
	where item = itemAt grid point

itemAt :: Grid t -> Point -> Maybe t
itemAt grid@(Grid array _ _) point@(Point x y)
	| containsPoint grid point = array !? itemIndex grid point
	| otherwise = Nothing
	
changeItemAt :: Grid t -> t -> Point -> Grid t
changeItemAt grid@(Grid array width height) newItem point = Grid (fromList newList) width height
	where
		newList = [if index == newItemIndex then newItem else item | (index, item) <- indexItemPairs]
		newItemIndex = itemIndex grid point
		indexItemPairs = Prelude.zip [0..] list
		list = toList array

changeItemsAt :: Grid t -> Vector t -> [Point] -> Grid t 
changeItemsAt grid indexes (point:points)
	| Prelude.length points == 1 = gridWithFirstItemChanged
	| otherwise = changeItemsAt gridWithFirstItemChanged (Data.Vector.tail indexes) points
	where
		gridWithFirstItemChanged = changeItemAt grid (Data.Vector.head indexes) point

changeGridAt :: Grid t -> Grid t -> Point -> Grid t
changeGridAt oldGrid@(Grid oldArray _ _) newGrid@(Grid newArray _ _) point
	| Data.Vector.length oldArray == 1 = changeItemAt oldGrid (Data.Vector.head oldArray) point
	| otherwise = changeItemsAt oldGrid oldArray pointList
	where
		pointList = [itemPoint newGrid index | index <- Prelude.take (Data.Vector.length newArray) [0..]]

flipAboutVertical :: Grid t -> Grid t
flipAboutVertical grid@(Grid _ width height) = Grid newArray width height
	where
		newArray = fromList [unsafeItemAt grid $ Point (width - x - 1) y | Point x y <- range origin $ maxPoint grid]

color :: Grid Color -> Color
color (Grid array _ _) = Data.Vector.head $ Data.Vector.filter (/= Empty) array

transpose :: Grid t -> Grid t
transpose grid@(Grid _ width height) = Grid newArray height width
	where
		newArray = fromList [unsafeItemAt grid point | point <- transposeRange origin $ maxPoint grid]

rotate90 = flipAboutVertical . transpose

rotate180 :: Grid t -> Grid t
rotate180 (Grid array width height) = Grid (Data.Vector.reverse array) width height

rotate270  = rotate90 . rotate180
