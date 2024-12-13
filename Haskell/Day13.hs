import AdventHelper
import Data.Tuple.Extra

data Game = Game { a :: (Int, Int), b :: (Int, Int), p :: (Int, Int) }
    deriving (Eq, Ord, Show)

parseInput :: [String] -> Game
parseInput [r1, r2, r3] = Game a b p
    where p1 = splitOnAnyOf ["X+", ", Y+"] r1
          a = both read (p1!!1, p1!!2)
          p2 = splitOnAnyOf ["X+", ", Y+"] r2
          b = both read (p2!!1, p2!!2)
          p3 = splitOnAnyOf ["X=", ", Y="] r3
          p = both read (p3!!1, p3!!2)
parseInput x = error "Input must consist of exactly three lines"

score :: Bool -> Game -> Int
score True (Game a b p) = score False $ Game a b (10000000000000 + fst p, 10000000000000 + snd p)
score False (Game a b p) = if' ((sc1 > 0) && (sc2 > 0)) (3 * sc1 + sc2) 0 
    where det = snd b * fst a - fst b * snd a
          dtb = snd p * fst a - fst p * snd a
          dta = snd b * fst p - fst b * snd p
          sc1 = if' (dta `mod` det == 0) (dta `div` det) 0
          sc2 = if' (dtb `mod` det == 0) (dtb `div` det) 0

batchLines :: [String] -> [[String]]
batchLines [] = []
batchLines ss = take 3 ss : batchLines (drop 4 ss)

main :: IO ()
main = do
    f <- readFile "../input/input13.txt"
    let l = lines f
    let gs = map parseInput $ batchLines l
    let p1 = sum $ map (score False) gs
    let p2 = sum $ map (score True) gs

    printSoln 13 p1 p2
