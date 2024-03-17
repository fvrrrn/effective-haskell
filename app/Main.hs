module Main where
import qualified MyLib (countNonPrintableCharacters)


main :: IO ()
main = do
  print (MyLib.countNonPrintableCharacters "\v\t\aHello\r\n")
