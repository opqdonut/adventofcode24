module Day22 where

import Data.List
import Data.Bits
import Data.Ord
import qualified Data.Map as M

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

prices init = map (`mod`10) $ take 2001 $ iterate secret init

type Quad = (Int,Int,Int,Int)

potentialSells :: Int -> M.Map Quad Int
potentialSells init = M.fromListWith (\_ x -> x) $ go ds (tail ps)
  where ps = prices init
        ds = zipWith (-) (tail ps) ps
        go (d1:d2:d3:d4:ds) (p1:p2:p3:p4:ps) = ((d1,d2,d3,d4),p4):go (d2:d3:d4:ds) (p2:p3:p4:ps)
        go _ _ = []

part2 inp = maximum $ M.elems $ foldl' (M.unionWith (+)) M.empty $ map potentialSells inp

main = do
  --print . part1 =<< slurp
  print . part2 =<< slurp
