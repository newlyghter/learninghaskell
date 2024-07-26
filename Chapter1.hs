module Chapter1 where

-- Logical solution

sumTo :: Integer -> Integer
sumTo x = sum [1..x]

-- Retarded Recursion solution

sumToR :: Integer -> Integer
sumToR 1 = 1
sumToR n = n + sumToR (n-1)

-- n^k using recursion

power :: Integer -> Integer -> Integer
power n 0 = 1
power n 1 = n
power n k = n * power n (k-1)

-- ilog3 using recursion
-- ilog3 n should be the number of times you can divide
-- given number by three (rounding down) before you hit 0

ilog3 :: Integer -> Integer
ilog3 0 = 0
ilog3 n = show "IDK BRO"
