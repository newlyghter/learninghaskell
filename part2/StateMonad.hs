import Control.Monad.Trans.State
import Control.Monad

-- adds i to the value of the counter
add :: Int -> State Int ()
add i = do old <- get
           put (old+i)

example :: State Int Int
example = do add 3
             value <- get
             add 1000
             put (value + 1)
             return value

findLargest :: Ord a => [a] -> State a ()
findLargest []     = return ()
findLargest (x:xs) = do
  modify (\y -> max x y)
  findLargest xs

findLargest' :: Ord a => [a] -> State a ()
findLargest' xs = mapM_ update xs
  where update x = modify (\y -> max x y)

remember :: a -> State [a] ()
remember x = modify (x:)

valuesAfterZero :: [Int] -> ((), [Int])
valuesAfterZero xs = runState (go xs) []
  where go :: [Int] -> State [Int] ()
        go (0:y:xs) = do remember y
                         go (y:xs)
        go (x:xs) = go xs
        go [] = return ()
