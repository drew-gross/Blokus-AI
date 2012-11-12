module Utilities(
	repeatedSingleDigits,
	maybeIndex,
	maybeHead,

	prompt,
	read1IndexedIndex,
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