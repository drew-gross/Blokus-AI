module Color
(
	Color(Yellow, Red, Green, Blue, Empty),
	coloredString
) where

import Display

data Color = Yellow | Red | Green | Blue | Empty deriving (Show, Eq)

colorBlack  = "\ESC[0;30m"
colorRed    = "\ESC[0;31m"
colorGreen  = "\ESC[0;32m"
colorYellow = "\ESC[0;33m"
colorBlue =   "\ESC[0;34m"

instance Display Color where
	display Empty = "."
	display color = coloredString "█" color

coloredString :: String -> Color -> String
coloredString string Red = colorRed ++ string ++ colorBlack
coloredString string Green = colorGreen ++ string ++ colorBlack
coloredString string Yellow = colorYellow ++ string ++ colorBlack
coloredString string Blue = colorBlue ++ string ++ colorBlack
coloredString string _ = string