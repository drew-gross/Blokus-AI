module Color
(
	Color(Yellow, Red, Green, Blue, Empty)
) where

import Utilities

data Color = Yellow | Red | Green | Blue | Empty deriving (Show, Eq)

instance ShowToUser Color where
	showToUser Yellow = "Y"
	showToUser Red = "R"
	showToUser Green = "G"
	showToUser Blue = "B"
	showToUser Empty = "."