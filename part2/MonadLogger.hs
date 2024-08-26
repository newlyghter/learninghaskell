module MonadLogger where

import Control.Monad

data Logger a = Logger [String] a
  deriving Show

msg :: String -> Logger ()
msg s = Logger [s] ()

instance Functor Logger where
  fmap f (Logger log x) = Logger log (f x)

instance Applicative Logger where
  pure = return
  (<*>) = ap

instance Monad Logger where
  return x = Logger [] x
  Logger la a >>= f = Logger (la++lb) b
    where Logger lb b = f a

nomsg :: a -> Logger a
nomsg x = return x

annotate :: String -> a -> Logger a
annotate s x = msg s >> return x

compute x = do
  a <- annotate "^2" (x*x)
  b <- annotate "+1" (a+1)
  annotate "*2" (b*2)

filterLog :: (Show a) => (a -> Bool) -> [a] -> Logger [a]
filterLog f [] = return []
filterLog f (x:xs)
  | f x        = do msg ("keeping "++show x)
                    xs' <- filterLog f xs
                    return (x:xs')
  | otherwise  = do msg ("dropping "++show x)
                    filterLog f xs
