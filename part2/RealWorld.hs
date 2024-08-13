module RealWorld where

import Control.Monad

questionnaire :: IO ()
questionnaire = do
    putStrLn "Write something!"
    s <- getLine
    putStrLn ("You wrote: "++s)

-- fetching words from URL
{-
main = do
  rsp <- simpleHTTP  (getRequest) "http://httpbin.org/..."
  body <- getResponseBody rsp
  forM_ (words body) $ \w -> do
    putStr "word: "
    putStrLn w
-}

query :: IO ()
query = do
  putStrLn "Write something!"
  s <- getLine
  let n = length s
  putStrLn ("You wrote "++show n++" characters")

askForALine :: IO String
askForALine = do
    putStrLn "Please give me a line"
    getLine

-- we can use "<-" to capture the value:
-- line <- askForALine
-- line is then line :: String and contains the value

ask :: String -> IO String
ask question = do
    putStrLn question
    getLine

-- "return" in Haskell is not keyword but a function:
-- return :: a -> IO
-- it take a value and turns it into an operation that produces the value

produceThree :: IO Int
produceThree = return 3

printThree :: IO ()
printThree = do
    three <- produceThree
    putStrLn (show three)

yesNoQuestion :: String -> IO Bool
yesNoQuestion question = do
    putStrLn question
    s <- getLine
    return (s == "Y")

-- return does not stop execution of an operation
-- last line decides the value that gets returned

produceTwo :: IO Int
produceTwo = do return 1
                return 2

{-
do ...
  x <- op
  return x

do ...
  op

is the same
-}

-- its a function so complex expressions should be in parenthesis
-- return (f x : xs) or return $ f x : xs

printDescription :: Int -> IO ()
printDescription n
  | even n    = putStrLn "even"
  | n==3      = putStrLn "three"
  | otherwise = print n

printList :: [Int] -> IO ()
printList [] = return ()
printList (x:xs) = do print x
                      printList xs

readAndSum :: Int -> IO Int
readAndSum 0 = return 0
readAndSum n = do
  i <- readLn
  s <- readAndSum (n-1)
  return (i+s)

-- functions from Control.Monad
{-
-- when b op performs op if b is true
when :: Bool -> IO () -> IO ()
-- unless b op performs op if b is false
unless :: Bool -> IO () -> IO ()
-- do something many times, collect results
replicateM :: Int -> IO a -> IO [a]
-- do something many times, throw away the results
replicateM_ :: Int -> IO a -> IO ()
-- do something for every list element
mapM :: (a -> IO b) -> [a] -> IO [b]
-- do something for every list element, throw away the results
mapM_ :: (a -> IO b) -> [a] -> IO ()
-- the same, but arguments flipped
forM  :: [a] -> (a -> IO b) -> IO [b]
forM_ :: [a] -> (a -> IO b) -> IO ()
-}

printList' :: [Int] -> IO ()
printList' = mapM_ print

readAndSum' :: (Read b, Num b) => Int -> IO b
readAndSum' n = do
  numbers <- replicateM n readLn
  return (sum numbers)