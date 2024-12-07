module AdventHelper where
import Data.List (foldl')
import Data.List.Split (splitOn)

splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

zipWithFn :: (a -> b) -> [a] -> [(a,b)]
zipWithFn fn as  = zip as (map fn as)

printSoln :: (Show a, Show b) => Integer -> a ->  b -> IO()
printSoln n p1 p2 = 
    do putStrLn ("Day " ++ show n)
       putStrLn ("  Part 1: " ++ show p1)
       putStrLn ("  Part 2: " ++ show p2)