module Display(
	Display,
	display,
	displayColored,
	displayNumberedList,
) where

import Color

class Display a where
	display :: a -> String
	displayColored :: Color -> a -> String
	displayColored color item = coloredString (display item) color

instance (Display a) => Display [a] where
	display list = concatMap display list ++ "\n"

instance Display Color where
	display Empty = "."
	display color = coloredString "█" color

instance Display Char where
	display char = [char]

displayNumberedListHelper :: Display t => [t] -> Int -> String
displayNumberedListHelper [] _ = ""
displayNumberedListHelper (x:xs) count = show count ++ "\n" ++ display x ++ displayNumberedListHelper xs (count + 1) 

displayNumberedList :: Display t => [t] -> String
displayNumberedList list = displayNumberedListHelper list 1