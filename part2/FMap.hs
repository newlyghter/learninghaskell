module FMap where

{-
map function:

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map g (x:xs) = g x : map g xs

it applies function 'g :: a -> b' to each element of a list of type [a]
it returns a list of tybe [b]

we can express it as (a -> b) -> ([a] -> [b])

map transforms functions to functions

-}

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

data Tree a = Leaf | Node a (Tree a) (Tree a)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node val left right) = Node (f val) (mapTree f left) (mapTree f right)

data Pair a = Pair a a
  deriving Show

instance Functor Pair where
    -- fmap f applies f to all values
    fmap f (Pair x y) = Pair (f x) (f y)

instance Foldable Pair where
    -- just like applying foldr over a list of length 2
    foldr f initialValue (Pair x y) = f x (f y initialValue)

-- an example function that uses both instances
doubleAndCount :: (Functor f, Foldable f) => f Int -> Int
doubleAndCount = sum . fmap (*2)

-- now fmap, foldr, length and minimum works, just like a list