module Day02 where

import Control.Applicative
import Data.List
import System.IO

slurp :: IO [[Int]]
slurp = map (map read . words) <$> lines <$> readFile "input-02"

safe xs = all small diffs
  where diffs = zipWith (-) (tail xs) xs
        sign = signum (head diffs)
        small d = signum d == sign && abs d >= 1 && abs d <= 3

part1 = length . filter safe

deletions [] = []
deletions (x:xs) = xs:map (x:) (deletions xs)

damped xs = any safe (deletions xs)

part2 = length . filter damped

main = do
  --print =<< part1 <$> slurp
  print =<< part2 <$> slurp
