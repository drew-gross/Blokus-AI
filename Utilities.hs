module Utilities(
	repeatedSingleDigits,
	prompt,
) where

import System.IO

repeatedSingleDigits = concat $ repeat $ [1..9] ++ [0]

prompt :: String -> IO String
prompt text = do
	putStr text
	hFlush stdout
	getLine