data Color = Yellow | Red | Green | Blue | Empty deriving (Show, Eq)
data Board = Board {array :: [Color], size :: Int} deriving (Show)
data Piece = Piece {colorArray :: [[Color]]}
data Player = Player {pieces :: [Piece], color :: Color}

defaultSize = 20
newBoard = (Board (take (defaultSize * defaultSize) $ repeat Empty) defaultSize)
newPlayer = ([[[True]], 
			 [[True, False], [True, True]],
			 [[True, True]],
			 [[True, True], [True, True]],
			 [[True, True, True]]], 
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
corners (Board board size) x y 
	| x < 0 || x > size || y < 0 || y > size = error "index out of range"
	| otherwise = (safeBoardAccess (Board board size) (x-1) (y-1)) ++ (safeBoardAccess (Board board size) (x-1) (y+1)) ++ (safeBoardAccess (Board board size) (x+1) (y-1)) ++ (safeBoardAccess (Board board size) (x+1) (y+1))

sides :: Board -> Int -> Int -> [Color]
sides (Board board size) x y
	| x < 0 || x >= size || y < 0 || y >= size = error "index out ofrange"
	| otherwise = (safeBoardAccess (Board board size) (x-1) (y)) ++ (safeBoardAccess (Board board size) (x+1) (y)) ++ (safeBoardAccess (Board board size) (x) (y-1)) ++ (safeBoardAccess (Board board size) (x) (y+1))

isOpenToColor :: Board -> Color -> Int -> Int -> Bool
isOpenToColor (Board board size) color 0 0 = board !! 0 == Empty
isOpenToColor (Board board size) color x y = (color `elem` (corners (Board board size) x y)) && (not (color `elem` sides (Board board size) x y)) && (board !! ((x*size) + y) == Empty)

addSquareToBoard :: Board -> Color -> Int -> Int -> Board
addSquareToBoard board color x y 
	| x < 0 || x >= size || y < 0 || y >= size = error "index out ofrange"
	| otherwise = Board (init $ fst (splitAt (boardIndex board x y) (array board)) ++ [color] ++ (snd $ splitAt (boardIndex board x y) (array board))) (size board)

addPieceToBoard :: Board -> Piece -> Int -> Int -> Int -> Board
addPieceToBoard board piece x y 0 =error "not implemented yet" 
addPieceToBoard _ _ _ _ rotation = error "not implemented yet"