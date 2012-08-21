module Utilities(
	initOrEmpty,
	removeItem,
	repeatedSingleDigits,
	concatTuple,
) where

initOrEmpty :: [a] -> [a]
initOrEmpty [] = []
initOrEmpty list = init list

safeAccess :: [a] -> Int -> [a]
safeAccess list index = if index < 0 || index >= length list
					then []
					else [list !! index]

removeItem :: Int -> [a] -> [a]
removeItem index list = let
	pair = splitAt index list
	in fst pair ++ (tail $ snd pair)

repeatedSingleDigits = concat $ repeat $ [1..9] ++ [0]

concatTuple tup = fst tup ++ snd tup