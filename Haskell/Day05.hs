import AdventHelper

import Data.List.Split ( splitOn )
import Data.Tuple.Extra

makeRule :: String -> (Int, Int)
makeRule s = both read (head ss, last ss)
    where ss = splitOn "|" s

makeBook :: String -> [Int]
makeBook s = map read ss
    where ss = splitOn "," s

bubbleOnce :: [(Int, Int)] -> [Int] -> [Int]
bubbleOnce rules (x:y:xs)
  | (y,x) `elem`  rules = y : bubbleOnce rules (x:xs)
  | otherwise = x : bubbleOnce rules (y:xs)
bubbleOnce _ [x] = [x]
bubbleOnce _ [] = []

bubbleSort :: [(Int, Int)] -> ([Int], Int) -> ([Int], Int)
bubbleSort rules (p, n)
    | p == p' = (p, n)
    | otherwise = bubbleSort rules (p', n+1)
    where p' = bubbleOnce rules p 

middle :: [a] -> a
middle [l] = l
middle l = middle $ tail $ init l

main :: IO ()
main = do   
    f <- readFile "../input/input05.txt"
    let rs = map makeRule $ takeWhile (/= "") $ lines f
    let bs = map makeBook $ drop 1 $ dropWhile (/= "") $ lines f
    let t = map (bubbleSort rs) $ zip bs $ repeat 0
    let p1 = sum $ map (middle . fst) $ filter (\x -> snd x == 0) t
    let p2 = sum $ map (middle . fst) $ filter (\x -> snd x /= 0) t

    printSoln 5 p1 p2
