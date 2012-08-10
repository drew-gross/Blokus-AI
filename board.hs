module Board
(
	Board(Board),
	boardAccess,
	boardIndex,
	isInBoard,
	addSquareToBoard,
	Color(Red, Yellow, Green, Blue, Empty),
	BoardPoint(BoardPoint, x, y),
	leftPoint,
	rightPoint,
	upPoint,
	downPoint,

	ulPoint,
	urPoint,
	dlPoint,
	drPoint,

	initOrEmpty,
	showToUser
) where
	
import Data.List.Split

class ShowToUser a where
	showToUser :: a -> String

instance (ShowToUser a) => ShowToUser [a] where
	showToUser list = concatMap showToUser list ++ "\n"

initOrEmpty :: [a] -> [a]
initOrEmpty [] = []
initOrEmpty list = init list

data Color = Yellow | Red | Green | Blue | Empty deriving (Show, Eq)

instance ShowToUser Color where
	showToUser Yellow = "Y"
	showToUser Red = "R"
	showToUser Green = "G"
	showToUser Blue = "B"
	showToUser Empty = "."
data Board = Board {array :: [Color], size :: Int} deriving (Show)

isInBoard :: Board -> BoardPoint -> Bool
isInBoard board (BoardPoint x y) = x >= 0 && x < size board && y >= 0 && y < size board

boardAccess :: Board -> BoardPoint -> Color
boardAccess board point
	| not $ isInBoard board point = error "index out of range"
	| otherwise = array board !! boardIndex board point

boardIndex :: Board -> BoardPoint -> Int
boardIndex board point = ((x point) * (size board)) + (y point)

addSquareToBoard :: Board -> Color -> BoardPoint -> Board
addSquareToBoard (Board array size) color (BoardPoint 0 0) = (Board ([color] ++ (tail array)) size)
addSquareToBoard board color point
	| not $ isInBoard board point = error "index out of range"
	| otherwise = 
		let splitBoard = splitAt (boardIndex board point) (array board)
		in Board ((initOrEmpty $ fst splitBoard) ++ [color] ++ snd splitBoard) (size board)

instance ShowToUser Board where
	showToUser board = showToUser $ splitEvery 20 (array board)

data BoardPoint = BoardPoint {x :: Int, y :: Int} deriving (Show)
leftPoint :: BoardPoint -> BoardPoint
leftPoint (BoardPoint x y) = BoardPoint (x - 1) y

rightPoint :: BoardPoint -> BoardPoint
rightPoint (BoardPoint x y) = BoardPoint (x + 1) y

upPoint :: BoardPoint -> BoardPoint
upPoint (BoardPoint x y) = BoardPoint x (y - 1)

downPoint :: BoardPoint -> BoardPoint
downPoint (BoardPoint x y) = BoardPoint x (y + 1)

ulPoint = upPoint . leftPoint
urPoint = upPoint . rightPoint
dlPoint = downPoint . leftPoint
drPoint = downPoint . rightPoint