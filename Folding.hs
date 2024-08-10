module Folding where

import Data.List
import Data.Ord

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f y []     = y
-- foldr f y (x:xs) = f x (foldr f y xs)
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- sumNumbers :: [Int] -> Int
-- sumNumbers xs == foldr (+) 0 xs

-- sorts lists by their length
sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (comparing length)