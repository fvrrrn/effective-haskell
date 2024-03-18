module MyLib (countNonPrintableCharacters, countNonPrintableCharactersInText) where
import qualified Data.Char
import Data.Text


countNonPrintableCharacters :: String -> Int
countNonPrintableCharacters =
  Prelude.length . Prelude.filter (not . Data.Char.isPrint)

-- pack :: String -> Text
countNonPrintableCharactersInText :: String -> Int
countNonPrintableCharactersInText =
  Data.Text.length . Data.Text.filter (not . Data.Char.isPrint) . pack

