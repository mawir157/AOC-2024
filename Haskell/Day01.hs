import AdventHelper

import Data.List
import Data.List.Split
import Data.Tuple.Extra

parseInput :: String -> (Int, Int)
parseInput s =  both read (head ss, last ss)
    where ss = splitOn " " s

part1 :: ([Int], [Int]) -> Int
part1 (lhs, rhs) = sum $ zipWith (\x y -> abs (x - y)) lhs rhs

countIf :: Eq a => [a] -> a -> Int
countIf xs x = length $ filter (== x) xs

part2 :: ([Int], [Int]) -> Int
part2 (lhs, rhs) = sum $ zipWith (*) lhs $ map (countIf rhs) lhs

main :: IO ()
main = do
    f <- readFile "../input/input01.txt"
    let s = lines f
    let q = map parseInput s
    let r = (sort $ map fst q, sort $ map snd q)
    printSoln 1 (part1 r) (part2 r)
