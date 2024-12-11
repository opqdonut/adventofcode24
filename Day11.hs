module Day11 where

import Data.List

parse :: String -> [Int]
parse = map read . words

slurp = parse <$> readFile "input-11"

step :: Int -> [Int]
step 0 = [1]
step n
  | even len = [read $ take (div len 2) digits, read $ drop (div len 2) digits]
  | otherwise = [n*2024]
  where digits = show n
        len = length digits

stepAll = concatMap step

part1 inp = length $ iterate stepAll inp !! 25

part2 inp = length $ iterate stepAll inp !! 75

main = do
  --print . part1 =<< slurp
  print . part2 =<< slurp -- Too slow, didn't complete in 5min even when compiled
