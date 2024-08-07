module Holes where

-- filtering list using a list of bools
keepElements :: [a] -> [Bool] -> [a]
keepElements xs bs = map fst (filter snd (zip xs bs))