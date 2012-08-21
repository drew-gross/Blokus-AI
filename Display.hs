module Display(
	Display,
	display,
	printDisplay,
	displayNumberedList,
	printDisplayNumberedList
) where

class Display a where
	display :: a -> String

instance (Display a) => Display [a] where
	display list = concatMap display list ++ "\n"

printDisplay :: Display a => a -> IO ()
printDisplay = putStrLn . display

displayNumberedListHelper :: Display t => [t] -> Int -> String
displayNumberedListHelper [] _ = ""
displayNumberedListHelper list count = show count ++ "\n" ++ (display $ head list) ++ displayNumberedListHelper (tail list) (count + 1) 

displayNumberedList :: Display t => [t] -> String
displayNumberedList list = displayNumberedListHelper list 1

printDisplayNumberedList :: Display t => [t] -> IO ()
printDisplayNumberedList = putStrLn . displayNumberedList