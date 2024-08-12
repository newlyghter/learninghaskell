module InfiniteList where

viitenumeroCheck :: [Int] -> Bool
viitenumeroCheck allDigits = mod (checksum+checkDigit) 10 == 0
  where (checkDigit:digits) = reverse allDigits
        multipliers = cycle [7,3,1]
        checksum = sum $ zipWith (*) multipliers digits