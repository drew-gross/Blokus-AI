module Color
(
	Color(Yellow, Red, Green, Blue, Empty)
) where

import Display

data Color = Yellow | Red | Green | Blue | Empty deriving (Show, Eq)

instance Display Color where
	display Yellow = "Y"
	display Red = "R"
	display Green = "G"
	display Blue = "B"
	display Empty = "."