safeAccess :: [a] -> Int -> [a]
safeAccess l i = if ((length l) <= i || i < 0)
					then []
					else [l !! i]

safeAccess2D :: [[a]] -> Int -> Int -> [a]
safeAccess2D l x y = safeAccess (if length (safeAccess l x) > 0 then (safeAccess l x) !! 0 else []) y

corners board x y = (safeAccess2D board (x-1) (y-1)) ++ (safeAccess2D board (x-1) (y+1)) ++ (safeAccess2D board (x+1) (y-1)) ++ (safeAccess2D board (x+1) (y+1))

sides board x y = (safeAccess2D board (x-1) (y)) ++ (safeAccess2D board (x+1) (y)) ++ (safeAccess2D board (x) (y-1)) ++ (safeAccess2D board (x) (y+1))


isOpen board 0 0 = not (board !! 0 !! 0)
isOpen board x y = (True `elem` (corners board x y)) && (not (True `elem` sides board x y))

board = take 20 (repeat (take 20 $ repeat False))