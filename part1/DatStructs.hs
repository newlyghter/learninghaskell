module DatStructs where

import qualified Data.Map as Map
import Data.Array

-- Create a Map from a list of key-value pairs
-- Map.fromList :: Ord k => [(k, a)] -> Map.Map k a

-- Insert a value into a map. Overrides any previous value with the same key.
-- Returns a new map. Does not mutate the given map.
-- Map.insert :: Ord k => k -> a -> Map.Map k a -> Map.Map k a

-- Get a value from a map using a key. Returns Nothing if the key was not present in the map.
-- Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a

-- An empty map
-- Map.empty :: Map.Map k a

withdraw :: String -> Int -> Map.Map String Int -> Map.Map String Int
withdraw account amount bank =
    case Map.lookup account bank of
        Nothing  -> bank
        Just sum -> Map.insert account (sum-amount) bank

-- withdraw :: String -> Int -> Map.Map String Int -> Map.Map String Int
-- withdraw account amount bank = Map.adjust (\x -> x-amount) account bank

-- array :: Ix i => (i, i) -> [(i, e)] -> Array i e
-- myArray :: Array Int String
-- myArray = array (7,11) [(7,"seven"), (8,"eight"), (9,"nine"), (10,"ten"), (11,"ELEVEN")]

-- listArray :: Ix i => (i,i) -> [e] -> Array i e
-- myArray :: Array Int String
-- myArray = listArray (7,11) ["seven", "eight", "nine", "ten", "ELEVEN"]

-- Array lookup
-- (!) :: Ix i => Array i e -> i -> e
-- Array update
-- (//) :: Ix i => Array i e -> [(i, e)] -> Array i e