module Utilities(
	ShowToUser,
	showToUser,
	initOrEmpty,
) where

class ShowToUser a where
	showToUser :: a -> String

instance (ShowToUser a) => ShowToUser [a] where
	showToUser list = concatMap showToUser list ++ "\n"

initOrEmpty :: [a] -> [a]
initOrEmpty [] = []
initOrEmpty list = init list