module JustMaybe where

login :: String -> Maybe String
login "f4bulous!" = Just "unicorn73"
login "swordfish" = Just "megahacker"
login _           = Nothing

perhapsMultiply :: Int -> Maybe Int -> Int
perhapsMultiply i Nothing = i
perhapsMultiply i (Just j) = i*j

intOrZero :: Maybe Int -> Int
intOrZero Nothing = 0
intOrZero (Just i) = i

safeHead :: [a] -> Maybe a
safeHead xs = if null xs then Nothing else Just (head xs)

headOrZero :: [Int] -> Int
headOrZero xs = intOrZero (safeHead xs)

-- getElement (Just i) gets the ith element (counting from zero) of a list, getElement Nothing gets the last element
getElement :: Maybe Int -> [a] -> a
getElement (Just i) xs = xs !! i
getElement Nothing xs = last xs
