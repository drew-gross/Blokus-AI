module Utilities(
	ShowToUser,
	showToUser,
	printToUser,
	initOrEmpty,
	removeItem,
) where

class ShowToUser a where
	showToUser :: a -> String

instance (ShowToUser a) => ShowToUser [a] where
	showToUser list = concatMap showToUser list ++ "\n"

printToUser :: ShowToUser a => a -> IO ()
printToUser = putStrLn . showToUser

initOrEmpty :: [a] -> [a]
initOrEmpty [] = []
initOrEmpty list = init list

removeItem :: Int -> [a] -> [a]
removeItem index list = let
	pair = splitAt index list
	in fst pair ++ (tail $ snd pair)