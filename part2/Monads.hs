module Monads where

import Data.List

(?>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing ?> _ = Nothing
Just x  ?> f = f x

{-
login :: String -> Maybe String
login "f4bulous!" = Just "unicorn73"
login "swordfish" = Just "megahacker"
login _           = Nothing


secret :: String -> Maybe String
secret "megahacker" = Just "I like roses"
secret _            = Nothing

stealSecret :: String -> Maybe String
stealSecret password =
    login password ?>
    secret ?>
    decorate
    where decorate s = Just ("Stole secret: " ++s)
-}

-- Get the value corresponding to a key from a key-value list.
-- lookup :: (Eq a) => a -> [(a, b)] -> Maybe b

increase :: Eq a => a -> Int -> [(a,Int)] -> Maybe [(a,Int)]
increase key val assocs =
    lookup key assocs ?>
    check ?>
    buildResult
  where check x
           | val < x   = Nothing
           | otherwise = Just x
        buildResult x = Just ((key,val) : delete (key,x) assocs)

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:xs) = Just xs

safeThird :: [a] -> Maybe a
safeThird xs = safeTail xs ?> safeTail ?> safeHead

safeNth :: Int -> [a] -> Maybe a
safeNth 0 xs = safeHead xs
safeNth n xs = safeTail xs ?> safeNth (n-1)

-- Logger definition
data Logger a = Logger [String] a
  deriving Show

(#>) :: Logger a -> (a -> Logger b) -> Logger b
Logger la a #> f = let Logger lb b = f a -- feed value to next step
                   in Logger (la++lb) b -- bundle result with all messages

(##>) :: Logger a -> Logger b -> Logger b
Logger la _ ##> Logger lb b = Logger (la++lb) b

filterLog :: (Eq a, Show a) => (a -> Bool) -> [a] -> Logger [a]
filterLog f [] = nomsg []
filterLog f (x:xs)
  | f x        = msg ("keeping "++show x) ##> filterLog f xs #> (\xs' -> nomsg (x:xs'))
  | otherwise  = msg ("dropping "++show x) ##> filterLog f xs

getVal :: Logger a -> a
getVal (Logger _ a) = a

getLog :: Logger a -> [String]
getLog (Logger s _) = s

-- primitive operations
nomsg :: a -> Logger a
nomsg = Logger []

annotate :: String -> a -> Logger a
annotate s = Logger [s]

msg :: String -> Logger ()
msg s = Logger [s] ()

square :: Int -> Logger Int
square val = annotate (show val ++ "^2") (val^2)

add :: Int -> Logger Int
add val = annotate (show val ++ "+1") (val+1)

double :: Int -> Logger Int
double val = annotate (show val ++ "*2") (val*2)

compute :: Int -> Logger Int
compute x =
    square x
    #> add
    #> double

validateUser :: String -> Logger Bool
validateUser "paul.atreides" = annotate "Valid user" True
validateUser "ninja" = nomsg True
validateUser u = annotate ("Invalid user: "++u) False

checkPassword :: String -> String -> Logger Bool
checkPassword "paul.atreides" "muad'dib" = annotate "Password ok" True
checkPassword "ninja"         ""         = annotate "Password ok" True
checkPassword _               pass       = annotate ("Password wrong: "++pass) False

login :: String -> String -> Logger Bool
login user password =
    validateUser user
    #>
    \valid -> if valid then checkPassword user password
                       else nomsg False