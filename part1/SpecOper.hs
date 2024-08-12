module SpecOper where

import Data.List

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (f.g) x ==> f (g x)
--
double x = 2*x
quadruple = double . double -- 2*(2*x) == 4*x
f = quadruple . (+1) -- 4*(x+1)
g = (+1) . quadruple -- 4*x+1
third = head . tail . tail

doTwice :: (a -> a) -> a -> a
doTwice f = f . f

-- let notEmpty x = not (null x)
-- in filter notEmpty [[1,2,3],[],[4]]
-- ==> [[1,2,3],[4]]
--
-- vs
--
-- filter (not . null) [[1,2,3],[],[4]]
-- ==> [[1,2,3],[4]]
--
--
-- ($) :: (a -> b) -> a -> b
--
-- f $ x == f x (same thing)
--
-- head (reverse "abcd") == head $ reverse "abcd"
--
-- advanced example:
-- we can rewrite stuff
--
-- reverse (map head (map reverse (["Haskell", "pro"] ++ ["dodo", "lyric"])))
--
-- (reverse . map head . map reverse) (["Haskell", "pro"] ++ ["dodo", "lyric"])
--
-- reverse . map head . map reverse $ ["Haskell", "pro"] ++ ["dodo", "lyric"]
--
-- apply list of functions on argument using map:
-- map ($ "string") [reverse, take 2, drop 2]

whatFollows c k string = map tail (filter match (map shorten (tails string)))
  where shorten s = take (k+1) s
        match sub = take 1 sub == [c]

whatFollows c k string = map tail (filter match (map (take (k+1)) (tails string)))
  where match sub = take 1 sub == [c]

whatFollows c k string = map tail . filter match . map (take (k+1)) $ tails string
  where match sub = take 1 sub == [c]

whatFollows c k string = map tail . filter (\sub -> take 1 sub == [c]) . map (take (k+1)) $ tails string

whatFollows c k = map tail . filter (\sub -> take 1 sub == [c]) . map (take (k+1)) . tails

whatFollows c k = map tail . filter ((==[c]) . take 1) . map (take (k+1)) . tails
