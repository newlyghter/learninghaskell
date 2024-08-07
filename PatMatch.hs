myhead :: [a] -> Maybe a
myhead [] = Nothing
myhead (first:rest) = Just first

mytail :: [a] -> [a]
mytail [] = []
mytail (first:rest) = rest

sumFirstTwo :: [Integer] -> Integer
sumFirstTwo (a:b:_) = a+b
sumFirstTwo _ = 0

describeList :: [Int] -> String
describeList []         = "an empty list"
describeList [x]     = "a list with one element"
describeList [x,y]   = "a list with two elements"
describeList (x:y:z:xs) = "a list with at least three elements"

startsWithZero :: [Integer] -> Bool
startsWithZero (0:xs) = True
startsWithZero (x:xs) = False
startsWithZero []     = False

sumNumbers :: [Int] -> Int
sumNumbers [] = 0
sumNumbers (x:xs) = x + sumNumbers xs

myMaximum :: [Int] -> Int
myMaximum [] = 0
myMaximum (x:xs) = go x xs
  where go biggest [] = biggest
        go biggest (x:xs) = go (max biggest x) xs

countNothings :: [Maybe a] -> Int
countNothings [] = 0
countNothings (Nothing : xs) = 1 + countNothings xs
countNothings (Just _ : xs) = countNothings xs

doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList (x:xs) = 2*x : doubleList xs
