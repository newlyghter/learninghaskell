module Partial where

between :: Integer -> Integer -> Integer -> Bool
between lo high x = x < high && x > lo
