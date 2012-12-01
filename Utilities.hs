module Utilities(
	repeatedSingleDigits,
	maybeIndex,
	maybeHead,

	prompt,
	read1IndexedIndex,
	combinationsOfLength
) where

import System.IO

repeatedSingleDigits = concat $ repeat $ [1..9] ++ [0]

maybeIndex :: [a] -> Int -> Maybe a
maybeIndex xs index
	| index < 0 = Nothing
	| index >= length xs = Nothing
	| otherwise = Just $ xs !! index

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead xs = Just $ head xs

prompt :: String -> IO String
prompt text = do
	putStr text
	hFlush stdout
	getLine

read1IndexedIndex :: String -> Int
read1IndexedIndex = (flip (-) 1) . read

combinationsOfLength :: Int -> a -> a -> [[a]]
combinationsOfLength 0 _ _ = []
combinationsOfLength 1 val1 val2 = [[val1], [val2]]
combinationsOfLength len val1 val2 = map (val1 :) (combinationsOfLength (len - 1) val1 val2) ++ map (val2 :) (combinationsOfLength (len - 1) val1 val2)