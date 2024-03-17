module MyLib (countNonPrintableCharacters) where
import Data.Char


countNonPrintableCharacters :: String -> Int
countNonPrintableCharacters =
  length . filter (not . isPrint)

