import AdventHelper

import Data.List.Split (splitOn)

parseInput :: String -> (Int, [Int])
parseInput s = (t, a)
    where ss = splitOn ": " s
          t = read $ head ss :: Int
          a = map  read $ splitOn " " $ ss!!1 :: [Int]

countDigits :: Int -> Int
countDigits x 
    | x == 0 = 0
    | otherwise = 1 + countDigits (x `div` 10)

concatInts :: Int -> Int -> Int
concatInts x y = x * (10^n) + y
    where n = countDigits y

countdown :: Int -> (Int, [Int]) -> Bool
countdown acc (target, [])  = acc == target
countdown acc (target, x:xs)
    | acc > target = False
    | otherwise  = s || p
    where s = countdown (acc+x) (target, xs)
          p = countdown (acc*x) (target, xs)

countdownWithConcat :: Int -> (Int, [Int]) -> Bool
countdownWithConcat acc (target, [])  = acc == target
countdownWithConcat acc (target, x:xs)
    | acc > target = False
    | otherwise  = s || p || c
    where s = countdownWithConcat (acc+x) (target, xs)
          p = countdownWithConcat (acc*x) (target, xs)
          c = countdownWithConcat (concatInts acc x) (target, xs)

main :: IO ()
main = do
    f <- readFile "../input/input07.txt"
    let is = map parseInput $ lines f
    let p1 = zipWithFn (countdown 0) is
    let part1 = sum $ map (fst .fst) $ filter snd p1
    let p2 = zipWithFn (countdownWithConcat 0) is
    let part2 = sum $ map (fst .fst) $ filter snd p2

    printSoln 7 part1 part2
