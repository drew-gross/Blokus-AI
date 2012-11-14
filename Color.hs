module Color
(
	Color(Yellow, Red, Green, Blue, Empty)
) where

import Display

data Color = Yellow | Red | Green | Blue | Empty deriving (Show, Eq)

colorBlack  = "\ESC[0;30m"
colorRed    = "\ESC[0;31m"
colorGreen  = "\ESC[0;32m"
colorYellow = "\ESC[0;33m"
colorBlue =   "\ESC[0;34m"

instance Display Color where
	display Red    = colorRed ++ "R" ++ colorBlack
	display Green  = colorGreen ++ "G" ++ colorBlack
	display Yellow = colorYellow ++ "Y" ++ colorBlack
	display Blue   = colorBlue ++ "B" ++ colorBlack
	display Empty  = "."