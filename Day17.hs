module Day17 where

import Data.Bits
import Debug.Trace
import Data.List

data Registers = Registers {regA::Int,regB::Int,regC::Int,regIP::Int,out::[Int]}
  deriving Show

combo _ 0 = 0
combo _ 1 = 1
combo _ 2 = 2
combo _ 3 = 3
combo s 4 = regA s
combo s 5 = regB s
combo s 6 = regC s
combo _ 7 = error "reserved"

data Op = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv
  deriving (Show,Read,Eq,Enum)

toOp :: Int -> Op
toOp = toEnum

nextIP s = s {regIP = regIP s + 2}

op :: Registers -> Op -> Int -> Registers
op s Adv arg = nextIP $ s { regA = div (regA s) (2^combo s arg) }
op s Bxl arg = nextIP $ s { regB = xor (regB s) arg }
op s Bst arg = nextIP $ s { regB = mod (combo s arg) 8 }
op s Jnz arg = case regA s of 0 -> nextIP s
                              _ -> s { regIP = arg }
op s Bxc _arg = nextIP $ s { regB = xor (regB s) (regC s) }
op s Out arg = nextIP $ s { out = out s ++ [combo s arg `mod` 8] }
op s Bdv arg = nextIP $ s { regB = div (regA s) (2^combo s arg) }
op s Cdv arg = nextIP $ s { regC = div (regA s) (2^combo s arg) }


step :: Registers -> [Int] -> Registers
step s mem = {-traceShow (toOp (mem!!i), mem !! succ i,s) $-} op s (toOp (mem !! i)) (mem !! succ i)
  where i = regIP s

run :: Registers -> [Int] -> Registers
run s mem
  | regIP s < 0 || regIP s > length mem - 2 = s
  | otherwise = run (step s mem) mem

{-
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
-}

example1 = run (Registers 729 0 0 0 []) [0,1,5,4,3,0]

{-
Register A: 28422061
Register B: 0
Register C: 0

Program: 2,4,1,1,7,5,1,5,4,2,5,5,0,3,3,0
-}

input = [2,4,1,1,7,5,1,5,4,2,5,5,0,3,3,0]
part1 = out $ run (Registers 28422061 0 0 0 []) input

disassemble [] =  []
disassemble (o:a:mem) = (toOp o,a):disassemble mem

--disassemble [2,4,1,1,7,5,1,5,4,2,5,5,0,3,3,0]
--
--(Bst,4) -- B = mod A 8
--(Bxl,1) -- B = xor B 1
--(Cdv,5) -- C = div A (2^B)
--             only the low 3 bits of C matter, which means bits B..B+2 of A
--             which means at most bits 8..10, which means only the lowest 4 3-bit words of A matter!
--(Bxl,5) -- B = xor B 5
--(Bxc,2) -- B = xor B C
--(Out,5) -- Out B
--(Adv,3) -- A = div A 8
--(Jnz,0)

prog :: Int -> [Int]
prog 0 = []
prog a = let b = xor (mod a 8) 1
             c = mod (div a (2^b)) 8
             out = xor 5 (xor b c)
         in out : prog (div a 8)

splitWords 0 = []
splitWords a = mod a 8 : splitWords (div a 8)

joinWords [] = 0
joinWords (w:ws) = w + 8 * joinWords ws

wordVals = [0..7]

initialGuesses output = filter (\x -> take 1 (prog (joinWords x)) == take 1 output) [[x,y,z,w] | x <- wordVals, y <- wordVals, z <- wordVals, w <- wordVals]

evolve output k guesses = filter (\x -> take k (prog (joinWords x)) == take k output) [g ++ [w] | g <- guesses, w <- wordVals]

reproduce output = go 1 inits
  where inits = initialGuesses output
        target = length output
        go i gs
          | i == target = gs
          | otherwise = go (i+1) (evolve output (i+1) gs)

part2 = minimum $ map joinWords $ reproduce input
