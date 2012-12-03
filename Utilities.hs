module Utilities(
	repeatedSingleDigits,

	maybeIndex,
	maybeHead,
	maybeRead,

	prompt,
	read1IndexedIndex,
	combinationsOfLength
) where

import System.IO
import Control.Applicative

repeatedSingleDigits = concat $ repeat $ [1..9] ++ [0]

maybeIndex :: [a] -> Int -> Maybe a
maybeIndex xs index
	| index < 0 = Nothing
	| index >= length xs = Nothing
	| otherwise = Just $ xs !! index

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead xs = Just $ head xs

maybeRead :: Read a => String -> Maybe a 
maybeRead s = case reads s of 
	[(x, "")] -> Just x 
	_         -> Nothing

prompt :: String -> IO String
prompt text = do
	putStr text
	hFlush stdout
	getLine

read1IndexedIndex :: String -> Int
read1IndexedIndex = (flip (-) 1) . read

combinationsOfLength :: Int -> [a] -> [[a]]
combinationsOfLength 0 _ = []
combinationsOfLength _ [] = []
combinationsOfLength 1 xs = (: []) <$> xs
combinationsOfLength n xs = concat $ (prependToCombinations (n - 1) xs) <$> xs

prependToCombinations :: Int -> [a] -> a -> [[a]]
prependToCombinations n xs x = prependToLists x $ combinationsOfLength n xs

prependToLists :: a -> [[a]] -> [[a]]
prependToLists x lists = map (x :) lists