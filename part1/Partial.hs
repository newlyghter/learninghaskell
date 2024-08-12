module Partial where

between :: Integer -> Integer -> Integer -> Bool
between lo high x = x < high && x > lo

-- (between 1 5) 2
-- ==> True
--
-- let f = between 1 5 in f 2
-- ==> True
--
-- map (between 1 3) [1,2,3]
-- ==> [False, True, False]
--
-- map (drop 1) ["Hello", "World!"]
-- ==> ["ello", "orld!"]
--
-- map (*2) [1,2,3]
-- ==> [2,4,6]
--
-- map (2*) [1,2,3]
-- ==> [2,4,6]
--
-- map (1/) [1,2,3,4,5]
-- ==> [1.0...]
--
-- zipWith (+) [0,2,5] [1,3,3]
-- ==> [1,5,8]
--
-- filter (\x -> x>7) [1,10,100]
-- ==> [10,100]
--
-- (\x -> x*x) 3
-- ==> 9
--
-- (\x -> reverse x == x) "ABBA"
-- ==> True
--
-- filter (\x -> reverse x == x) ["ABBA", "ACDC", "otto", "anna"]
-- ==> ["ABBA","otto","anna"]
--
-- (\x y -> x^2+y^2) 2 3
-- ==> 13
