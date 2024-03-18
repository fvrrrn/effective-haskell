module MyLib (countNonPrintableCharacters, countNonPrintableCharactersInText) where
import Data.Char
import Data.Text


countNonPrintableCharacters :: String -> Int
countNonPrintableCharacters =
  Prelude.length . Prelude.filter (not . isPrint)

-- pack :: String -> Text
countNonPrintableCharactersInText :: String -> Int
countNonPrintableCharactersInText =
  Data.Text.length . Data.Text.filter (not . isPrint) . pack

