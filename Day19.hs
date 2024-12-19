module Day19 where

import Data.List
import Debug.Trace
import Data.Array

parse :: String -> ([String],[String])
parse s = (towels,problems)
  where stowels:"":problems = lines s
        towels = words $ filter (/=',') stowels

slurp = parse <$> readFile "input-19"

possible towels prob = arr!0
  where n = length prob
        arr = array (0,n) $ [(i,p i) | i <- [0..n]]
        p i
          | i==n = 1
          | otherwise = let s = drop i prob
                        in case [i + length t | t <- towels, t `isPrefixOf` s]
                           of [] -> 0
                              is -> sum $ map (arr!) is

exampleTowels = ["r", "wr", "b", "g", "bwu", "rb", "gb", "br"]
exampleProblems = ["brwrr", "bggr", "gbbr", "rrbgbr", "ubwu", "bwurrg", "brgr", "bbrgwb"]

part1 (ts,ps) = length (filter ((0<) . possible ts) ps)
part2 (ts,ps) = sum $ map (possible ts) ps

main = do
  --print . part1 =<< slurp
  print . part2 =<< slurp
