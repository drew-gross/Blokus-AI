module Utilities(
	initOrEmpty,
	removeItem,
	repeatedSingleDigits,
	concatTuple,
	prompt,
) where

import System.IO

initOrEmpty :: [a] -> [a]
initOrEmpty [] = []
initOrEmpty list = init list

safeAccess :: [a] -> Int -> [a]
safeAccess list index = if index < 0 || index >= length list
					then []
					else [list !! index]

removeItem :: Int -> [a] -> [a]
removeItem index list = let
	(first, second) = splitAt index list
	in first ++ (tail $ second)

repeatedSingleDigits = concat $ repeat $ [1..9] ++ [0]

concatTuple (first, second) = first ++ second

prompt :: String -> IO String
prompt text = do
	putStr text
	hFlush stdout
	getLine