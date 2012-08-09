import Debug.Trace
import Board


isInPiece :: Piece -> BoardPoint -> Bool
isInPiece piece (BoardPoint x y) = x >= 0 && x < width piece && y >= 0 && y < height piece

plus :: BoardPoint -> BoardPoint -> BoardPoint
plus (BoardPoint x1 y1) (BoardPoint x2 y2) = BoardPoint (x1 + x2) (y1 + y2)

data Piece = Piece {colorArray :: [[Color]]} deriving (Show)
width :: Piece -> Int
width (Piece colorArray) = length $ head colorArray

height :: Piece -> Int
height (Piece colorArray) = length colorArray

prevPoint :: Piece -> BoardPoint -> BoardPoint
prevPoint piece (BoardPoint 0 y) = BoardPoint (width piece) (y-1)
prevPoint piece boardPoint = BoardPoint (x boardPoint - 1) (y boardPoint)

maxPoint :: Piece -> BoardPoint
maxPoint piece = BoardPoint ((width piece) - 1) ((height piece) - 1)

pieceAccess :: Piece -> BoardPoint -> Color
pieceAccess piece (BoardPoint x y)
	| not $ isInPiece piece (BoardPoint x y) = error "index out of range"
	| otherwise = (colorArray piece) !! y !! x

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
safeAccess list index = if index < 0 || index >= length list
					then []
					else [list !! index]

safeBoardAccess :: Board -> BoardPoint -> [Color]
safeBoardAccess board point
	| not $ isInBoard board point = []
	| otherwise = [boardAccess board point]

corners :: Board -> BoardPoint -> [Color]
corners board point
	| not $ isInBoard board point = error "index out of range"
	| otherwise = (safeBoardAccess board (ulPoint point)) ++ (safeBoardAccess board (urPoint point)) ++ (safeBoardAccess board (drPoint point)) ++ (safeBoardAccess board (dlPoint point))

sides :: Board -> BoardPoint -> [Color]
sides board point
	| not $ isInBoard board point = error "index out of range"
	| otherwise = (safeBoardAccess board $ leftPoint point) ++ (safeBoardAccess board $ rightPoint point) ++ (safeBoardAccess board $ upPoint point) ++ (safeBoardAccess board $ downPoint point)

isOpenToColor :: Board -> Color -> BoardPoint -> Bool
isOpenToColor board color (BoardPoint 0 0) = boardAccess board (BoardPoint 0 0) == Empty
isOpenToColor board color point = (color `elem` (corners board point)) && (not (color `elem` sides board point)) && ((boardAccess board point) == Empty)

addPieceSquareToBoard :: Board -> Piece -> BoardPoint -> BoardPoint -> Board
addPieceSquareToBoard board piece boardLocation (BoardPoint 0 0) = addSquareToBoard board (pieceAccess piece (BoardPoint 0 0)) boardLocation
addPieceSquareToBoard board piece boardLocation pieceLocation = 
	let updatedBoard = (addSquareToBoard board (pieceAccess piece pieceLocation) (boardLocation `plus` pieceLocation))
	in addPieceSquareToBoard updatedBoard piece boardLocation (prevPoint piece pieceLocation)

addPieceToBoard :: Board -> Piece -> BoardPoint -> Int -> Board
addPieceToBoard board piece boardPoint 0 = traceShow boardPoint $ addPieceSquareToBoard board piece boardPoint (maxPoint piece)
addPieceToBoard _ _ _ rotation = error "not implemented yet"

main = do
	x <- getLine
	y <- getLine
	piece <- getLine
	putStr $ showToUser $ addPieceToBoard newBoard ((pieces newPlayer) !! read piece) (BoardPoint (read x) (read y)) 0
