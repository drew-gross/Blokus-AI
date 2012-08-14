module Utilities(
	ShowToUser,
	showToUser,
	initOrEmpty,
	stupidDivision
) where

class ShowToUser a where
	showToUser :: a -> String

instance (ShowToUser a) => ShowToUser [a] where
	showToUser list = concatMap showToUser list ++ "\n"

initOrEmpty :: [a] -> [a]
initOrEmpty [] = []
initOrEmpty list = init list

stupidDivision :: Int -> Int -> Int -> Bool -> (Int, Bool)
stupidDivision numerator denominator previous done
	| numerator < denominator = (previous, True)
	| otherwise = stupidDivision (numerator - denominator) denominator (previous + 1) False