import AdventHelper

import Data.List
import Data.Maybe

data Dir = N | E | S | W
     deriving (Read, Show, Enum, Eq, Ord)
type Grid = [String]
type Guard = ((Int, Int), Dir)

rotate :: Dir -> Dir
rotate N = E
rotate E = S
rotate S = W
rotate W = N

findGuard :: Int -> Grid -> Guard
findGuard _ [] = ((0,0), N)
findGuard n (s:ss)
    | '^' `elem` s = ((n, fromJust (elemIndex '^' s)), N)
    | otherwise = findGuard (n+1) ss

getCharAt :: Grid -> (Int,Int) -> Maybe Char
getCharAt g (r,c) 
    | (r < 0) || (r >= length g) = Nothing
    | (c < 0) || (c >= length (head g)) = Nothing
    | otherwise  = Just $ (g!!r)!!c

step :: Guard -> (Int, Int)
step ((r,c), N) = (r-1, c)
step ((r,c), E) = (r, c+1)
step ((r,c), S) = (r+1, c)
step ((r,c), W) = (r, c-1)

move ::(Int, Int) -> Grid -> Guard -> Guard
move q grid (p, d)
    | isNothing ch = (p, d)
    | p' == q = (p, rotate d)
    | fromJust ch == '#' = (p, rotate d)
    | otherwise = (p', d)
    where p' = step (p, d)
          ch = getCharAt grid p'

takeUntilStationary :: Eq a => [a] -> [a]
takeUntilStationary [] = []
takeUntilStationary [x] = [x]
takeUntilStationary (x:y:xs)
    | x == y = [x]
    | otherwise = x : takeUntilStationary (y:xs)

part1 :: Grid -> Guard -> Int
part1 grid g = length $ nub $ map fst path
    where path = takeUntilStationary $ iterate (move (-1,-1) grid) g

part2 :: Grid -> Guard -> (Int, Int) -> Bool
part2 grid g q = length maxPath == 10000
    where path = takeUntilStationary $ iterate (move q grid) g
          maxPath = take 10000 path

getOpenCells :: Grid -> (Int, Int) ->  Bool
getOpenCells g p
    | isNothing c = False
    | fromJust c == '.' = True
    | otherwise = False
    where c = getCharAt g p
 
main :: IO ()
main = do   
    f <- readFile "../input/input06.txt"
    let grid = lines f
    let guard = findGuard 0 grid
    let p1 = part1 grid guard
    let pairs = [(x,y) | x <- [0..130], y <- [0..130]]
    let openPos = filter (getOpenCells grid) pairs
    let p2 = length $ filter (part2 grid guard) openPos

    printSoln 6 p1 p2
