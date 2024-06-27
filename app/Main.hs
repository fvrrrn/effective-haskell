{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=>" #-}
{-# HLINT ignore "Use >>" #-}
module Main where

returnHello :: IO (IO String)
returnHello = return (return "Hello")

returnIOWA :: IO (IO wa) -> IO wa
returnIOWA io = io >>= id

returnArrayIO :: [IO wa] -> IO [wa]
returnArrayIO [] = return []
returnArrayIO (x:xs) = do
  x' <- x
  xs' <- returnArrayIO xs
  return $ x' : xs'

-- main :: IO ()
-- main =
--   returnHello >>= \io -> io >>= putStrLn
-- main :: IO ()
-- main =
--   returnIOWA (return (return "Hello from IOWA")) >>= putStrLn

main :: IO ()
main =
  returnIOWA (return (return "Hello from IOWA")) >>= putStrLn


