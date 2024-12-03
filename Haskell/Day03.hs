import Data.List.Split
import Text.Regex.Posix

import AdventHelper

part1Regex :: String
part1Regex = "mul\\([0-9]+,[0-9]+\\)"
part2Regex :: String
part2Regex = "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)"

part1 :: String -> Int
part1 s = product p
    where ss = splitOn "," $ drop 4 $ init s
          p = map read ss :: [Int]

flag :: Bool -> String -> (Bool, Bool)
flag _ "do()" = (True, False)
flag _ "don't()" = (False, False)
flag b _ = (b, True)

part2 :: (Bool, Int) -> [String] -> Int
part2 (_, t) [] = t
part2 (on, t) (s:ss) = part2 (on', t') ss
    where (on', mul) = flag on s
          t' = if' (on' && mul) (t + (part1 s)) t

main :: IO()
main = do
    f <- readFile "../input/input03.txt"
    let s = concat $ lines f
    let r1 = getAllTextMatches $ s =~ part1Regex :: [String]
    let p1 = sum $ map part1 r1
    let r2 = getAllTextMatches $ s =~ part2Regex :: [String]
    let p2 = part2 (True, 0) r2

    printSoln 3 p1 p2
