module InfiniteList where

viitenumeroCheck :: [Int] -> Bool
viitenumeroCheck allDigits = mod (checksum+checkDigit) 10 == 0
  where (checkDigit:digits) = reverse allDigits
        multipliers = cycle [7,3,1]
        checksum = sum $ zipWith (*) multipliers digits

firstPower :: Int -> Int -> Int
firstPower n more = head . filter (>more) $ map (n^) [0..]

everySecond :: [a] -> [a]
everySecond [] = []
everySecond (x:y:xs) = x : everySecond xs

foldl'Int :: (Int -> Int -> Int) -> Int -> [Int] -> Int
foldl'Int f z [] = z
foldl'Int f 0 (x:xs) = foldl'Int f (f 0 x) xs
foldl'Int f z (x:xs) = foldl'Int f (f z x) xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z [] = z
foldl' f z (x:xs) = let z' = f z x
                    in seq z' (foldl' f z' xs)

-- newtype expects one constructor with exactly one field
newtype Money = Cents Int