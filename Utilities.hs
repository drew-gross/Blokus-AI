module Utilities(
	ShowToUser,
	showToUser,
	printToUser,
	showNumberedListToUser,
	printNumberedListToUser,
	initOrEmpty,
	removeItem,
) where

class ShowToUser a where
	showToUser :: a -> String

instance (ShowToUser a) => ShowToUser [a] where
	showToUser list = concatMap showToUser list ++ "\n"

printToUser :: ShowToUser a => a -> IO ()
printToUser = putStrLn . showToUser

showNumberedListToUserHelper :: ShowToUser t => [t] -> Int -> String
showNumberedListToUserHelper [] _ = ""
showNumberedListToUserHelper list count = show count ++ "\n" ++ (showToUser $ head list) ++ showNumberedListToUserHelper (tail list) (count + 1) 

showNumberedListToUser :: ShowToUser t => [t] -> String
showNumberedListToUser list = showNumberedListToUserHelper list 1

printNumberedListToUser :: ShowToUser t => [t] -> IO ()
printNumberedListToUser = putStrLn . showNumberedListToUser

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