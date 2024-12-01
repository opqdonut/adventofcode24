module Day01 where

import Control.Applicative
import Data.List
import System.IO

parseLine :: String -> (Int,Int)
parseLine s = let [a,b] = words s in (read a, read b)

slurp = map parseLine <$> lines <$> readFile "input-01"

a -* b = abs (a-b)

part1 i = sum $ zipWith (-*) (sort as) (sort bs)
  where (as,bs) = unzip i

similarity is i = i * length (filter (==i) is)

part2 inp = sum $ map (similarity bs) as
  where (as,bs) = unzip inp

main =
  --print =<< part1 <$> slurp
  print =<< part2 <$> slurp

