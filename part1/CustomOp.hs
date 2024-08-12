module CustomOp where

(<+>) :: [Int] -> [Int] -> [Int]
xs <+> ys = zipWith (+) xs ys

(+++) :: String -> String -> String
a +++ b = a ++ " " ++ b