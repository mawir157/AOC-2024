import AdventHelper
import Data.List (nub)
import qualified Data.Map as M

type Pos = (Int, Int)
type Ants = M.Map Char [Pos]

insertMap :: Ants -> (Char, Pos) -> Ants
insertMap m (c, p)
    | M.member c m = M.adjust (\x -> x ++ [p]) c m
    | otherwise = M.insert c [p] m

findAntennae :: Int -> [String] -> [(Char, Pos)]
findAntennae _ [] = []
findAntennae r (s:ss) =  row ++ findAntennae (r+1) ss
    where row = findAntennae' (r, 0) s

findAntennae' :: Pos -> String -> [(Char, Pos)]
findAntennae' _ [] = []
findAntennae' (r,c) (x:xs)
    | x == '.' = findAntennae' (r, c+1) xs
    | otherwise = (x, (r, c)) : findAntennae' (r, c+1) xs

nodePair :: (Int, Int) -> (Pos, Pos) -> [Pos]
nodePair lims ((r1,c1), (r2,c2)) = filter (inBounds lims) [(r1+dr, c1+dc), (r2-dr, c2-dc)]
    where dr = r1-r2
          dc = c1-c2

nodeLine :: (Int, Int) -> (Pos, Pos) -> [Pos]
nodeLine lims ((r1,c1), (r2,c2)) = f ++ b
    where dr = r1-r2
          dc = c1-c2
          f = takeWhile (inBounds lims) [(r1 + n*dr, c1 + n*dc) | n <- [0,1..]]
          b = takeWhile (inBounds lims) [(r1 - n*dr, c1 - n*dc) | n <- [0,1..]]


inBounds :: (Int, Int) -> Pos -> Bool
inBounds (r_lim, c_lim) (r,c) =(r >= 0) && (r < r_lim) && (c >= 0) && (c < c_lim)


findAntiNodes1 :: (Int, Int) -> [Pos] -> [Pos]
findAntiNodes1 lims ps = concatMap (nodePair lims) pairs
    where pairs = [ (i,j) | i <- ps, j <- ps,  i /= j ]

findAntiNodes2 :: (Int, Int) -> [Pos] -> [Pos]
findAntiNodes2 lims ps = concatMap (nodeLine lims) pairs
    where pairs = [ (i,j) | i <- ps, j <- ps,  i /= j ]

main :: IO ()
main = do
    f <- readFile "../input/input08.txt"
    let gd = lines f
    let as = findAntennae 0 gd
    let m = M.empty
    let am = M.elems $ foldl insertMap m as
    let p1 = concatMap (findAntiNodes1 (length gd, length $ head gd)) am
    let p2 = concatMap (findAntiNodes2 (length gd, length $ head gd)) am

    printSoln 8 (length $ nub p1) (length $ nub p2)
