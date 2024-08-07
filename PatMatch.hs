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
