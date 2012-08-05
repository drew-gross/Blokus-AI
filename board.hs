data Color = Yellow | Red | Green | Blue | Empty deriving (Show, Eq)
data Board = Board [[Color]] deriving (Show)

newBoard = take 20 $ repeat $ take 20 $ repeat Empty

safeAccess :: [a] -> Int -> [a]
safeAccess l i = if ((length l) <= i || i < 0)
					then []
					else [l !! i]

safeAccess2D :: Board -> Int -> Int -> [Color]
safeAccess2D (Board board) x y = safeAccess (if length (safeAccess board x) > 0 then (safeAccess board x) !! 0 else []) y

corners :: Board -> Int -> Int -> [Color]
corners board x y = (safeAccess2D board (x-1) (y-1)) ++ (safeAccess2D board (x-1) (y+1)) ++ (safeAccess2D board (x+1) (y-1)) ++ (safeAccess2D board (x+1) (y+1))

sides :: Board -> Int -> Int -> [Color]
sides board x y = (safeAccess2D board (x-1) (y)) ++ (safeAccess2D board (x+1) (y)) ++ (safeAccess2D board (x) (y-1)) ++ (safeAccess2D board (x) (y+1))

isOpenToColor :: Board -> Color -> Int -> Int -> Bool
isOpenToColor (Board board) color 0 0 = board !! 0 !! 0 == Empty
isOpenToColor board color x y = (color `elem` (corners board x y)) && (not (color `elem` sides board x y))