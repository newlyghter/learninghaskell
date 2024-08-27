module ControlStructures where

import Control.Monad

{-
when :: Monad m => Bool -> m () -> m ()         conditional operation
unless :: Monad m => Bool -> m () -> m ()       same, but condition is flipped
replicateM :: Monad m => Int -> m a -> m [a]    do something many times
replicateM_ :: Monad m => Int -> m a -> m ()    same, but ignore the results
mapM :: Monad m => (a -> m b) -> [a] -> m [b]   do something on a list's elements
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()   same, but ignore the results
forM  :: Monad m => [a] -> (a -> m b) -> m [b]  mapM but arguments reversed
forM_ :: Monad m => [a] -> (a -> m b) -> m ()   same, but ignore the results
-}

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

firsts :: [[a]] -> Maybe [a]
firsts xs = forM xs safeHead

