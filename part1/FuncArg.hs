module FuncArg where

applyTo1 :: (Int -> Int) -> Int
applyTo1 f = f 1

doTwice :: (a -> a) -> a -> a
doTwice f x = f (f x)

makeCool :: String -> String
makeCool str = "WOW " ++ str ++ "!"

addThree :: Int -> Int
addThree x = x + 3

-- map addThree [1,2,3]
-- ==> [4,5,6]
-- map :: (a -> b) -> [a] -> [b]

positive :: Int -> Bool
positive x = x > 0

-- filter :: (a -> Bool) -> [a] -> [a]
-- filter positive [0,1,-1,3,-3]
-- ==> [1,3]

wrapJust xs = map Just xs
