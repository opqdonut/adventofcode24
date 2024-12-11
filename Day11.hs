module Day11 where

import Control.Monad.Fix
import Data.List
import Numeric
import Data.Array

parse :: String -> [Int]
parse = map read . words

slurp = parse <$> readFile "input-11"

countDigits :: Int -> Int
countDigits 0 = 1
countDigits n = go 0 n
  where go d 0 = d
        go d n = go (d+1) (div n 10)

splitDigits :: Int -> Int -> (Int,Int)
splitDigits digits n = divMod n (10^digits)

step :: Int -> [Int]
step 0 = [1]
step n
  | even len = let (a,b) = splitDigits (div len 2) n in [a,b]
  | otherwise = [n*2024]
  where len = countDigits n

stepAll = concatMap step

naive n inp = length $ iterate stepAll inp !! n

part1 inp = naive 25 inp

lenAfter :: ((Int,Int) -> Int) -> (Int,Int) -> Int
lenAfter f (0,_) = 1
lenAfter f (s,0) = f ((s-1),1)
lenAfter f (s,n)
  | even digs = let (a,b) = splitDigits (div digs 2) n
                in f (s-1, a) + f (s-1, b)
  | otherwise = f (s-1, n*2024)
  where digs = countDigits n

lenAfterRecursive = fix lenAfter

recursive n inp = sum $ map (\k -> lenAfterRecursive (n,k)) inp

memoArray rng f = lookup
  where arr = array rng [(i,f lookup i) | i <- range rng]
        lookup i
          | inRange rng i = arr ! i
          | otherwise = f lookup i

lenAfterMemo = memoArray ((0,0),(75,100000)) lenAfter

memoized n inp = sum $ map (\k -> lenAfterMemo (n,k)) inp

part2 inp = memoized 75 inp

main = do
  --print . part1 =<< slurp
  print . part2 =<< slurp
