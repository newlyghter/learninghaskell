module Tuples where

-- fst :: (a, b) -> a
-- snd :: (a, b) -> b

-- zip :: [a] -> [b] -> [(a, b)]
-- unzip :: [(a, b)] -> ([a], [b])
-- partition :: (a -> Bool) -> [a] -> ([a], [a])

-- zip [1,2,3] [True,False,True]
-- ==> [(1,True),(2,False),(3,True)]
-- unzip [("Fred",1), ("Jack",10), ("Helen",13)]
-- ==> (["Fred","Jack","Helen"],[1,10,13])
-- partition (>0) [-1,1,-4,3,2,0]
-- ==> ([1,3,2],[-1,-4,0])

-- swap :: (a,b) -> (b,a)
-- swap (x,y) = (y,x)

-- sum all numbers that are paired with True
sumIf :: [(Bool, Int)] -> Int
sumIf [] = 0
sumIf((True,x):xs) = x + sumIf xs
sumIf((False,_):xs) = sumIf xs