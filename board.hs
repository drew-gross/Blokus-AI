import Data.List.Split

class ShowToUser a where
	showToUser :: a -> String

instance (ShowToUser a) => ShowToUser [a] where
	showToUser list = concatMap showToUser list ++ "\n"

data BoardPoint = BoardPoint {x :: Int, y :: Int} deriving (Show)
isInBoard :: Board -> BoardPoint -> Bool
isInBoard board point = (x point) >= 0 || (x point) < size board || (y point) >= 0 || (y point) < size board

leftPoint :: BoardPoint -> BoardPoint
leftPoint (BoardPoint x y) = BoardPoint (x - 1) y

rightPoint :: BoardPoint -> BoardPoint
rightPoint (BoardPoint x y) = BoardPoint (x + 1) y

upPoint :: BoardPoint -> BoardPoint
upPoint (BoardPoint x y) = BoardPoint x (y - 1)

downPoint :: BoardPoint -> BoardPoint
downPoint (BoardPoint x y) = BoardPoint x (y + 1)

data Color = Yellow | Red | Green | Blue | Empty deriving (Show, Eq)

instance ShowToUser Color where
	showToUser Yellow = "Y"
	showToUser Red = "R"
	showToUser Green = "G"
	showToUser Blue = "B"
	showToUser Empty = "."

data Board = Board {array :: [Color], size :: Int} deriving (Show)

instance ShowToUser Board where
	showToUser board = showToUser $ splitEvery 20 (array board)

data Piece = Piece {colorArray :: [[Color]]} deriving (Show)
width :: Piece -> Int
width piece = length $ colorArray piece !! 0

prevPoint :: Piece -> BoardPoint -> BoardPoint
prevPoint piece (BoardPoint 0 y) = BoardPoint (width piece) (y-1)
prevPoint piece boardPoint = BoardPoint (x boardPoint - 1) (y boardPoint)

maxPoint :: Piece -> BoardPoint
maxPoint piece = BoardPoint ((width piece) - 1) (length ((colorArray piece) !! 0))

data Player = Player {pieces :: [Piece], color :: Color} deriving (Show)

defaultSize = 20
newBoard = (Board (take (defaultSize * defaultSize) $ repeat Empty) defaultSize)
newPlayer = (Player 
			(
			 [
			 Piece [[Red]], 
			 Piece [[Red, Empty], [Red, Red]],
			 Piece [[Red, Red]],
			 Piece [[Red, Red], [Red, Red]],
			 Piece [[Red, Red, Red]]
			 ]
			)
			 Red)

safeAccess :: [a] -> Int -> [a]
safeAccess l i = if ((length l) <= i || i < 0)
					then []
					else [l !! i]

boardIndex :: Board -> BoardPoint -> Int
boardIndex board point = ((x point) * (size board)) + (y point)

boardAccess :: Board -> BoardPoint -> Color
boardAccess board point
	| not $ isInBoard board point = error "index out of range"
	| otherwise = array board !! (((x point)*(size board)) + (y point))

safeBoardAccess :: Board -> BoardPoint -> [Color]
safeBoardAccess board point
	| not $ isInBoard board point = []
	| otherwise = [boardAccess board point]

corners :: Board -> BoardPoint -> [Color]
corners board point
	| not $ isInBoard board point = error "index out of range"
	| otherwise = (safeBoardAccess board (BoardPoint ((x point)-1) ((y point)-1))) ++ (safeBoardAccess board (BoardPoint ((x point)-1) ((y point)+1))) ++ (safeBoardAccess board (BoardPoint ((x point)+1) ((y point)-1))) ++ (safeBoardAccess board (BoardPoint ((x point)+1) ((y point)+1)))

sides :: Board -> BoardPoint -> [Color]
sides board point
	| not $ isInBoard board point = error "index out of range"
	| otherwise = (safeBoardAccess board $ leftPoint point) ++ (safeBoardAccess board $ rightPoint point) ++ (safeBoardAccess board $ upPoint point) ++ (safeBoardAccess board $downPoint point)

isOpenToColor :: Board -> Color -> BoardPoint -> Bool
isOpenToColor board color (BoardPoint 0 0) = (array board) !! 0 == Empty
isOpenToColor board color point = (color `elem` (corners board point)) && (not (color `elem` sides board point)) && ((boardAccess board point) == Empty)

initOrEmpty :: [a] -> [a]
initOrEmpty [] = []
initOrEmpty list = init list

addSquareToBoard :: Board -> Color -> BoardPoint -> Board
addSquareToBoard board color point
	| not $ isInBoard board point = error "index out of range"
	| otherwise = Board ((initOrEmpty $ fst $ splitAt (boardIndex board point) (array board)) ++ [color] ++ (snd $ splitAt (boardIndex board point) (array board))) (size board)

addPieceSquareToBoard :: Board -> Piece -> BoardPoint -> BoardPoint -> Board
addPieceSquareToBoard board piece boardLocation (BoardPoint 0 0) = addSquareToBoard board 
																					((colorArray piece) !! (x boardLocation) !! (y boardLocation))
																					boardLocation
addPieceSquareToBoard board piece boardLocation pieceLocation = addPieceSquareToBoard (addSquareToBoard 
																					   board 
																					   ((colorArray piece) !! (x boardLocation) !! (y boardLocation)) 
																					   (BoardPoint ((x boardLocation) + (x pieceLocation)) ((y boardLocation) + (y pieceLocation)))) piece boardLocation (prevPoint piece pieceLocation)

addPieceToBoard :: Board -> Piece -> BoardPoint -> Int -> Board
addPieceToBoard board piece boardPoint 0 = (addPieceSquareToBoard board piece boardPoint) (maxPoint piece)
addPieceToBoard _ _ _ rotation = error "not implemented yet"

main = do
	x <- getLine
	y <- getLine
	piece <- getLine
	putStr $ showToUser $ addPieceToBoard newBoard ((pieces newPlayer) !! read piece) (BoardPoint (read x) (read y)) 0
