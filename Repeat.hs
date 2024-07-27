module Repeat where

repeatString :: Integer -> String -> String
repeatString n str = repeatHelper n str ""

repeatHelper :: Integer -> String -> String -> String
repeatHelper n str result = if (n==0)
                            then result
                            else repeatHelper (n-1) str (result++str)
