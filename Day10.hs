module Day10 where

import qualified Data.Map as M
import Data.Char
import Data.Maybe
import Data.List

type Map = M.Map (Int,Int) Int

parse :: String -> Map
parse s = M.fromList [ ((x,y),digitToInt c) | (y,row) <- zip [0..] (lines s)
                                            , (x,c) <- zip [0..] row ]

slurp = parse <$> readFile "input-10"

get m (x,y) = fromMaybe 20 $ M.lookup (x,y) m

neighbours (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

ascendingPaths m (x,y) = go (get m (x,y)) [(x,y)]
  where go 9 cs = cs
        go l cs = go (l+1) [ c | c <- nub $ concatMap neighbours cs
                               , get m c == l+1 ]

example = parse $ unlines ["89010123"
                          ,"78121874"
                          ,"87430965"
                          ,"96549874"
                          ,"45678903"
                          ,"32019012"
                          ,"01329801"
                          ,"10456732"]

part1 :: Map -> Int
part1 m = sum [ length (ascendingPaths m c) | (c,0) <- M.assocs m ]

-- This counts distinct paths because we don't nub the coordinates.
-- If we can get to the same coordinate via two paths, we keep two copies of the coordinate
-- in the state. Then, if the path forks again, we'll have four paths etc.
distinctPaths m (x,y) = go (get m (x,y)) [(x,y)]
  where go 9 cs = cs
        go l cs = go (l+1) [ c | c <- concatMap neighbours cs
                               , get m c == l+1 ]

part2 :: Map -> Int
part2 m = sum [ length (distinctPaths m c) | (c,0) <- M.assocs m ]

main = do
  --print . part1 =<< slurp
  print . part2 =<< slurp
