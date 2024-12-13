import AdventHelper
import Data.Function (on)
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.List.Split ( splitOn )
import Data.Tuple.Extra ( both )

parseInput :: String -> [Int]
parseInput s = map read $ splitOn " " s

splitDigit :: Int -> (Int,Int)
splitDigit x =  both read (l,r)
    where s = show x
          l = take (length s `div` 2) s
          r = drop (length s `div` 2) s

blink' :: [(Int, Int)] -> [(Int, Int)] 
blink' [] = []
blink' ((x,c):ss) = x' ++ blink' ss
    where x'
            | 0 == x = [(1,c)]
            | even n = [(l,c), (r,c)]
            | otherwise = [(2024*x, c)]
                where n = length $ show x
                      (l,r) = splitDigit x

blinkN' :: Int -> [(Int, Int)] -> [(Int, Int)] 
blinkN' 0 xs = xs
blinkN' n xs = blinkN' (n-1) $ groupStones $ blink' xs

groupStones :: [(Int, Int)] -> [(Int, Int)]
groupStones = map (\l -> (fst . head $ l, sum $ map snd l)) . groupBy ((==) `on` fst)
          . sortBy (comparing fst)

main :: IO ()
main = do
    f <- readFile "../input/input11.txt"
    let g = parseInput $ head $ lines f

    let p1 = sum $ map snd $ blinkN' 25 $ map (,1) g
    let p2 = sum $ map snd $ blinkN' 75 $ map (,1) g

    printSoln 11 p1 p2