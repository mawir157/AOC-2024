import AdventHelper

import Data.List
import Data.List.Split

rotate :: [[a]] -> [[a]]
rotate a = reverse $ transpose a

scanRows :: String -> [String] -> Int
scanRows _ [] = 0
scanRows s (r:rs) = for + rev + scanRows s rs
    where for = length (splitOn s r) - 1
          rev = length (splitOn (reverse s) r) - 1

findWord :: String -> [String] -> Int
findWord word grid = sum $ map (scanRows word) [grid, grid', dirg, dirg']
    where grid' = rotate grid
          dirg = rotate $ padGrid (length grid) grid
          dirg' = rotate $ padGrid (length (rotate grid)) (rotate grid)

padGrid :: Int -> [String] -> [String]
padGrid _ [] = []
padGrid n (s:ss) = (replicate pre '0' ++ s ++ replicate post '0') : padGrid n ss
    where post = length ss
          pre = (n-1) - post
          
triples :: [a] -> [[a]]
triples (x:y:z:rest) = [x,y,z] : triples (y:z:rest)
triples _ = []

hasCross :: [String] -> Bool
hasCross [['M',_,'M'],[_,'A',_],['S',_,'S']] = True
hasCross [['M',_,'S'],[_,'A',_],['M',_,'S']] = True
hasCross [['S',_,'M'],[_,'A',_],['S',_,'M']] = True
hasCross [['S',_,'S'],[_,'A',_],['M',_,'M']] = True
hasCross _ = False

main :: IO ()
main = do
    f <- readFile "../input/input04.txt"
    let s = lines f
    let part1 = scanRows "XMAS" s
    let ts = concatMap triples . map transpose $ triples s

    printSoln 1 (findWord "XMAS" s) (length (filter hasCross ts))
