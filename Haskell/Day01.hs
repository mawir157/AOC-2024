import Data.List
import Data.List.Split

parseInput :: String -> (Int, Int)
parseInput s = (l,r)
    where ss = splitOn "   " s
          l = read (head ss) :: Int
          r = read (last ss) :: Int

part1 :: ([Int], [Int]) -> Int
part1 (lhs, rhs) = sum $ zipWith (\x y -> abs (x - y)) lhs rhs

countIf :: Eq a => [a] -> a -> Int
countIf xs x = length $ filter (== x) xs

part2 :: ([Int], [Int]) -> Int
part2 (lhs, rhs) = sum $ zipWith (*) lhs rhs'
    where rhs' = map (countIf rhs) lhs

main :: IO ()
main = do
  f <- readFile "../input/input01.txt"
  let s = lines f
  let q = map parseInput s
  let r = (sort $ map fst q, sort $ map snd q)
  putStr "Show 1: "
  print $ part1 r
  putStr "Show 2: "
  print $ part2 r
