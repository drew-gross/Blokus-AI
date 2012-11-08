module Utilities(
	repeatedSingleDigits,
	prompt,
	maybeIndex
) where

import System.IO

repeatedSingleDigits = concat $ repeat $ [1..9] ++ [0]

maybeIndex :: [a] -> Int -> Maybe a
maybeIndex xs index
	| index < 0 = Nothing
	| index >= length xs = Nothing
	| otherwise = Just $ xs !! index

prompt :: String -> IO String
prompt text = do
	putStr text
	hFlush stdout
	getLine