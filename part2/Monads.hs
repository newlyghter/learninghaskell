module Monads where

import Data.List
import qualified Data.Map as Map
import Data.IntMap (findWithDefault)

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

data Bank = Bank (Map.Map String Int)
  deriving Show

-- apply a function to one value in a map
-- Map.adjust :: Ord k => (a -> a) -> k -> Map.Map k a -> Map.Map k a

deposit :: String -> Int -> Bank -> Bank
deposit accountName amount (Bank accounts) =
  Bank (Map.adjust (\x -> x+amount) accountName accounts)

-- fetch the value corresponding to a key from a map,
-- or a default value in case the key does not exist
-- Map.findWithDefault :: Ord k => a -> k -> Map.Map k a -> a

withdraw :: String -> Int -> Bank -> (Int, Bank)
withdraw accountName amount (Bank accounts) =
  let -- balance is 0 for nonexistent accounts
    balance = Map.findWithDefault 0 accountName accounts
    -- can't withdraw over balance
    withdrawal = min amount balance
    newAccounts = Map.adjust (\x -> x-withdrawal) accountName accounts
  in (withdrawal, Bank newAccounts)

share :: String -> String -> String -> Bank -> Bank
share from to1 to2 bank =
  let (amount,bank1) = withdraw from 100 bank
      half = div amount 2
      rest = amount - half
      bank2 = deposit to1 half bank1
      bank3 = deposit to2 rest bank2
  in bank3

-- finally monads
-- class Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b

{-
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>= _ = Nothing
Just x >>= f = f x
-}

{-
instance  Monad Maybe  where
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing

    (Just _) >>  k      = k
    Nothing  >>  _      = Nothing

    return x            = Just x
-}

increaseM :: Eq a => a -> Int -> [(a,Int)] -> Maybe [(a,Int)]
increaseM key val assocs =
    lookup key assocs >>=
    check >>=
    buildResult
  where check x
           | val < x   = Nothing
           | otherwise = return x
        buildResult x = return ((key,val) : delete (key,x) assocs)

safeNthM :: Int -> [a] -> Maybe a
safeNthM 0 xs = safeHead xs
safeNthM n xs = do t <- safeTail xs
                   safeNthM (n-1) t


