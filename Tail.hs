-- Not tail recursive
sumNumbers1 :: [Int] -> Int
sumNumbers1 [] = 0
sumNumbers1 (x:xs) = x + sumNumbers1 xs

-- Tail recursive version
sumNumbers2 :: [Int] -> Int
sumNumbers2 xs = go 0 xs
  where go sum [] = sum
        go sum (x:xs) = go (sum+x) xs

doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList (x:xs) = 2*x : doubleList xs