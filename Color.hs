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
	display Red    = colorRed ++ "█" ++ colorBlack
	display Green  = colorGreen ++ "█" ++ colorBlack
	display Yellow = colorYellow ++ "█" ++ colorBlack
	display Blue   = colorBlue ++ "█" ++ colorBlack
	display Empty  = "."