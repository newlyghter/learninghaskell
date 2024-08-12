module Palindrome where

palindrome :: String -> Bool
palindrome str = str == reverse str

palindromes :: Int -> [String]
palindromes n = filter palindrome (map show [1..n])
