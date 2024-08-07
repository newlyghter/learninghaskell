module ListWrangle where

-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- dropWhile :: (a -> Bool) -> [a] -> [a]
--
-- takeWhile even [2,4,1,2,3] ==> [2,4]
-- dropWhile even [2,4,1,2,3] ==> [1,2,3]
--
-- elem 3 [1,2,3] ==> True
-- elem 4 [1,2,3] ==> False

findSubstring :: String -> String -> String
findSubstring chars = takeWhile (\x -> elem x chars)
                      . dropWhile (\x -> not $ elem x chars)

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- id :: a -> a
-- id 3 ==> 3
--
-- const :: a -> b -> a returns first argument
-- const 3 True ==> 3
-- const 3 0 ==> 3
-- map (const 5) [1,2,3,4] ==> [5,5,5,5]
-- filter (const True) [1,2,3,4] ==> [1,2,3,4]
--
-- (:) :: a -> [a] -> [a]
-- 1:[] ==> [1]
-- 1:[2,3] ==> [1,2,3]
--
descend :: Int -> [Int]
descend 0 = []
descend n = n : descend (n-1)

iterate2 f 0 x = [x]
iterate2 f n x = x : iterate2 f (n-1) (f x)

split :: Char -> String -> [String]
split c [] = []
split c xs = start : split c (drop 1 rest)
  where start = takeWhile (/=c) xs
        rest = dropWhile (/=c) xs
