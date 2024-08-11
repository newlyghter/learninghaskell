module AlgDatatypes where
import Prelude hiding (lookup)

-- data Bool = True | False
-- data Ordering = LT | EQ | GT

data Color = Red | Green | Blue

rgb :: Color -> [Double]
rgb Red = [1,0,0]
rgb Green = [0,1,0]
rgb Blue = [0,0,1]

data Report = ConstructReport Int String String

reportContents :: Report -> String
reportContents (ConstructReport id title contents) = contents

setReportContents :: String -> Report -> Report
setReportContents contents (ConstructReport id title _contents) = ConstructReport id title contents

data Card = Joker | Heart Int | Club Int | Spade Int | Diamond Int
  deriving Show

data Described a = Describe a String

getValue :: Described a -> a
getValue (Describe x _) = x

getDescription :: Described a -> String
getDescription (Describe _ desc) = desc

-- data List a = Empty | Node a (List a)
--   deriving Show

-- lhead :: List a -> a
-- lhead (Node h _) = h

-- ltail :: List a -> List a
-- ltail (Node _ t) = t

-- lnull :: List a -> Bool
-- lnull Empty = True
-- lnull _ = False

-- llength :: List a -> Int
-- llength Empty = 0
-- llength (Node _ t) = 1 + llength t

data Tree a = Node a (Tree a) (Tree a) | Empty
  deriving Show

example :: Tree Int
example = Node 0 (Node 1 (Node 2 Empty Empty)
                          (Node 3 Empty Empty))
                  (Node 4 Empty Empty)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)

lookup :: Int -> Tree Int -> Bool
lookup x Empty = False
lookup x (Node y l r)
  | x < y = lookup x l
  | x > y = lookup x r
  | otherwise = True

insert :: Int -> Tree Int -> Tree Int
insert x Empty = Node x Empty Empty
insert x (Node y l r)
  | x < y = Node y (insert x l) r
  | x > y = Node y l (insert x r)
  | otherwise = Node y l r

data Person = MkPerson { name :: String, age :: Int, town :: String, state :: String, profession :: String}
  deriving Show

people :: [Person]
people = [ MkPerson "Jane Doe" 21 "Houston" "Texas" "Engineer"
         , MkPerson "Maija Meikäläinen" 35 "Rovaniemi" "Finland" "Engineer"
         , MkPerson "Mauno Mutikainen" 27 "Turku" "Finland" "Mathematician"
         ]

query :: [Person] -> [Person]
query []     = []
query (x:xs)
  | state x == "Finland" && profession x == "Engineer" =
      x : query xs
  | otherwise = query xs