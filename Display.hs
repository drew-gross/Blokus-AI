module Display(
	Display,
	display,
	printDisplay,
	displayNumberedList,
) where

import Control.Applicative

class Display a where
	display :: a -> String

instance (Display a) => Display [a] where
	display list = unlines $ display <$> list

printDisplay :: Display a => a -> IO ()
printDisplay = putStrLn . display

displayNumberedListHelper :: Display t => [t] -> Int -> String
displayNumberedListHelper [] _ = ""
displayNumberedListHelper (x:xs) count = show count ++ "\n" ++ display x ++ displayNumberedListHelper xs (count + 1) 

displayNumberedList :: Display t => [t] -> String
displayNumberedList list = displayNumberedListHelper list 1