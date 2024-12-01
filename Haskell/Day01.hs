import Data.List
import Data.List.Split

parseInput :: String -> (Int, Int)
parseInput s = (l,r)
    where ss = splitOn "   " s
          l = read (head ss) :: Int
          r = read (last ss) :: Int

part1 :: [(Int, Int)] -> Int
part1 xs = sum $ zipWith (\x y -> abs (x - y)) lhs rhs
    where lhs = sort $ map fst xs
          rhs = sort $ map snd xs

countIf :: Eq a => [a] -> a -> Int
countIf xs x = length $ filter (== x) xs

part2 :: [(Int, Int)] -> Int
part2 xs = sum $ zipWith (*) lhs rhs
    where lhs = map fst xs
          rhs = map (countIf (map snd xs)) lhs

main :: IO ()
main = do
  f <- readFile "../input/input01.txt"
  let s = lines f
  let q = map parseInput s
  putStr "Show 1: "
  print $ part1 q
  putStr "Show 2: "
  print $ part2 q
