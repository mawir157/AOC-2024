import AdventHelper

import Data.List.Split

parseInput ::String -> [Int]
parseInput s = map read $ splitOn " " s

diff :: [Int] -> [Int]
diff [x] = []
diff (x:y:xs) = [x-y] ++ diff (y:xs)

valid :: Bool -> [Int] -> Bool
valid _ [] = True
valid b (x:xs) = if' pass (valid b xs) False
    where pass = ((x > 0) == b) && ((abs x) >= 1) && ((abs x) <= 3)

part1 :: [Int] -> Bool
part1 v = valid (head d > 0) d
    where d = diff v

part2 :: [Int] -> Bool
part2 vs = or $ map (part2' vs) [-1..(length vs)]
    where part2' xs n = part1 (take n xs ++ drop (n+1) xs)

main :: IO()
main = do
    f <- readFile "../input/input02.txt"
    let vs = map parseInput $ lines f
    let p1 = length $ filter part1 vs
    let p2 = length $ filter part2 vs
    printSoln 2 p1 p2
