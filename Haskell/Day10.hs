import AdventHelper
import Data.List (nub)
import Data.Char (digitToInt)

type Pos = (Int, Int)

parseInput :: [String] -> [[Int]]
parseInput = map (map digitToInt)

isStart :: [[Int]] -> Pos -> Bool
isStart g (r,c) = g!!r!!c == 0

nbrs :: [[Int]] -> Pos -> [Pos]
nbrs g (r,c) = n ++ e ++ s ++ w
    where l = length g
          i = g!!r!!c + 1
          n = if'((r-1 >= 0) && (i == g!!(r-1)!!c)) [(r-1, c)] [] 
          e = if'((c+1 <  l) && (i == g!!r!!(c+1))) [(r, c+1)] [] 
          s = if'((r+1 <  l) && (i == g!!(r+1)!!c)) [(r+1, c)] [] 
          w = if'((c-1 >= 0) && (i == g!!r!!(c-1))) [(r, c-1)] [] 

ends :: [[Int]] -> Pos -> [Pos]
ends g (r,c)
    | i == 9 = [(r,c)]
    | null ns = []
    | otherwise = concatMap (ends g) ns
    where i = g!!r!!c
          ns = nbrs g (r,c)

main :: IO ()
main = do
    f <- readFile "../input/input10.txt"
    let g = parseInput $ lines f
    let d = length g - 1 
    let ss = filter (isStart g) [(x,y) | x <- [0..d], y <- [0..d]]
    let p1 = sum $ map (length. nub . ends g) ss 
    let p2 = sum $ map (length . ends g) ss 

    printSoln 10 p1 p2
