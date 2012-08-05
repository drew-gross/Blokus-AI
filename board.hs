data BoardPoint = BoardPoint {x :: Int, y :: Int} deriving (Show)
data Color = Yellow | Red | Green | Blue | Empty deriving (Show, Eq)
data Board = Board {array :: [Color], size :: Int} deriving (Show)
data Piece = Piece {colorArray :: [[Color]]} deriving (Show)
width :: Piece -> Int
width piece = length $ colorArray piece !! 0

prevPoint :: Piece -> BoardPoint -> BoardPoint
prevPoint piece (BoardPoint 0 y) = BoardPoint (width piece) (y-1)
prevPoint piece boardPoint = BoardPoint (x boardPoint - 1) (y boardPoint)

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

boardIndex :: Board -> Int -> Int -> Int
boardIndex board x y = (x * (size board)) + y

boardAccess :: Board -> Int -> Int -> Color
boardAccess board x y
	| x < 0 || x > size board || y < 0 || y > size board = error "index out of range"
	| otherwise = array board !! ((x*(size board)) + y)

safeBoardAccess :: Board -> Int -> Int -> [Color]
safeBoardAccess board x y
	| y < 0 || y >= (size board) || x < 0 || x >= (size board) = []
	| otherwise = [boardAccess board x y]

corners :: Board -> Int -> Int -> [Color]
corners board x y 
	| x < 0 || x > (size board) || y < 0 || y > (size board) = error "index out of range"
	| otherwise = (safeBoardAccess board (x-1) (y-1)) ++ (safeBoardAccess board (x-1) (y+1)) ++ (safeBoardAccess board (x+1) (y-1)) ++ (safeBoardAccess board (x+1) (y+1))

sides :: Board -> Int -> Int -> [Color]
sides board x y
	| x < 0 || x >= (size board) || y < 0 || y >= (size board) = error "index out ofrange"
	| otherwise = (safeBoardAccess board (x-1) (y)) ++ (safeBoardAccess board (x+1) (y)) ++ (safeBoardAccess board (x) (y-1)) ++ (safeBoardAccess board (x) (y+1))

isOpenToColor :: Board -> Color -> Int -> Int -> Bool
isOpenToColor board color 0 0 = (array board) !! 0 == Empty
isOpenToColor board color x y = (color `elem` (corners board x y)) && (not (color `elem` sides board x y)) && ((array board) !! ((x* (size board)) + y) == Empty)

addSquareToBoard :: Board -> Color -> Int -> Int -> Board
addSquareToBoard board color x y 
	| x < 0 || x >= (size board) || y < 0 || y >= (size board) = error "index out ofrange"
	| otherwise = Board (init $ fst (splitAt (boardIndex board x y) (array board)) ++ [color] ++ (snd $ splitAt (boardIndex board x y) (array board))) (size board)

addPieceSquareToBoard :: Board -> Piece -> BoardPoint -> BoardPoint -> Board
addPieceSquareToBoard board piece boardLocation (BoardPoint 0 0) = addSquareToBoard board ((colorArray piece) !! (x boardLocation) !! (y boardLocation)) (x boardLocation) (y boardLocation)
addPieceSquareToBoard board piece boardLocation pieceLocation = addPieceSquareToBoard (addSquareToBoard board ((colorArray piece) !! (x boardLocation) !! (y boardLocation)) ((x boardLocation) + (x pieceLocation)) ((y boardLocation) + (y pieceLocation))) piece boardLocation (prevPoint piece pieceLocation)

addPieceToBoard :: Board -> Piece -> BoardPoint -> Int -> Board
addPieceToBoard board piece boardPoint 0 = (addPieceSquareToBoard board piece boardPoint) (BoardPoint 0 0)
addPieceToBoard _ _ _ rotation = error "not implemented yet"