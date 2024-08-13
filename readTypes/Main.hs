module Main where

import ReadTypes

main :: IO ()
main = do ts <- readTypes "."
          mapM_ putStrLn ts