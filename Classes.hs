module Classes where

data Color = Black | White
  deriving Show

instance Eq Color where
    Black == Black = True
    White == White = True
    _     == _     = False

class Size a where
    empty :: a
    size :: a -> Int
    sameSize :: a -> a -> Bool

instance Size (Maybe a) where
    empty = Nothing
    size Nothing = 0
    size (Just a) = 1
    sameSize x y = size x == size y

instance Size [a] where
    empty = []
    size xs = length xs
    sameSize x y = size x == size y

class Example a where
    example :: a
    examples :: [a]
    examples = [example]

instance Example Int where
    example = 1
    examples = [0,1,2]

instance Example Bool where
    example = True

data IntPair = IntPair Int Int
    deriving Show

instance Eq IntPair where
    IntPair a1 a2 == IntPair b1 b2 = a1==b1 && a2==b2

instance Ord IntPair where
    IntPair a1 a2 <= IntPair b1 b2
        | a1<b1     = True
        | a1>b1     = False
        | otherwise = a2<=b2

data Pair a = MakePair a a
    deriving Show

instance Eq a => Eq (Pair a) where
    (MakePair x y) == (MakePair a b) = x==a && y==b

class Check a where
    check :: a -> Bool

instance Check Int where
    check x = x > 0

instance Check a => Check [a] where
    check = all check

checkAll :: Check a => [a] -> Bool
checkAll = all check
