import AdventHelper
import Data.Bits
import Data.List.Split ( splitOn )

type Machine = (Int, Int, Int)
type Program = [Int]

parseMachine :: [String] -> Machine
parseMachine [l1,l2,l3] = (a,b,c)
    where a = read $ drop 12 l1
          b = read $ drop 12 l2
          c = read $ drop 12 l3

parseProgram :: String -> [Int]
parseProgram s = map read $ splitOn "," (drop 9 s)

combo :: Machine -> Int -> Int
combo (a,b,c) 4 = a
combo (a,b,c) 5 = b
combo (a,b,c) 6 = c
combo _ n = n

run :: Program -> (Machine, Int, [Int]) -> (Machine, Int, [Int])
run p ((a,b,c), ptr, out)
    | ptr >= length p = ((a,b,c), ptr, out)
    | ins == 0 =  run p ((a `shiftR` com, b, c), ptr', out)
    | ins == 1 =  run p ((a, b `xor` opr, c), ptr', out)
    | ins == 2 =  run p ((a, com `mod` 8, c), ptr', out)
    | ins == 3 =  run p ((a, b, c), if' (a == 0) ptr' opr, out)
    | ins == 4 =  run p ((a, b `xor` c, c), ptr', out)
    | ins == 5 =  run p ((a, b, c), ptr', out ++ [com `mod` 8])
    | ins == 6 =  run p ((a, a `shiftR` com, c), ptr', out)
    | ins == 7 =  run p ((a, b, a `shiftR` com), ptr', out)
    where ins = p!!ptr
          opr = p!!(ptr+1)
          com = combo (a,b,c) opr
          ptr' = ptr + 2

main :: IO()
main = do      
    f <- readFile "../input/input17.txt"
    let ls = lines f
    let cpu = parseMachine $ take 3 ls
    let prg = parseProgram $ ls!!4
    let (_,_,out) = run prg (cpu, 0, [])

    printSoln 17 out 0
