{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Color(
	Color,
	yellow,
	red,
	green,
	blue,
	empty,
	coloredString
) where

newtype Color = Color Int deriving (Eq,Ord,Enum, Show)
(yellow:red:green:blue:empty:_) = [Color 1 ..]


colorBlack  = "\ESC[0;30m"
colorRed    = "\ESC[0;31m"
colorGreen  = "\ESC[0;32m"
colorYellow = "\ESC[0;33m"
colorBlue =   "\ESC[0;34m"

coloredString :: String -> Color -> String
coloredString string color 
	| color == red = colorRed ++ string ++ colorBlack
	| color == green = colorGreen ++ string ++ colorBlack
	| color == yellow = colorYellow ++ string ++ colorBlack
	| color == blue = colorBlue ++ string ++ colorBlack
	| otherwise = string