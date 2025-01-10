module Day22 where

import Data.Bits

mix :: Int -> Int -> Int
mix = xor

prune :: Int -> Int
prune n = mod n 16777216

secret :: Int -> Int
secret n = n4
  where n2 = prune (mix n (n*64))
        n3 = prune (mix n2 (div n2 32))
        n4 = prune (mix n3 (n3*2048))

secret2000 n = iterate secret n !! 2000

slurp = map read . lines <$> readFile "input.22"

part1 = sum . map secret2000

main = do
  print . part1 =<< slurp
