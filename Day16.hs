module Day16 where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace

type Pos = (Int,Int)
type Paths = S.Set Pos

parse :: String -> (Pos,Pos,Paths)
parse s = (start,end,S.fromList paths)
  where coords = [((x,y),c) | (y,line) <- zip [0..] (lines s)
                            , (x,c) <- zip [0..] line]
        start = head [pos | (pos,'S') <- coords]
        end = head [pos | (pos,'E') <- coords]
        paths = [(x,y) | ((x,y),c) <- coords, c /= '#']

slurp = parse <$> readFile "input-16"
example1 = parse <$> readFile "example1-16"
example1a = parse <$> readFile "example1a-16"

data Dir = U|L|D|R
  deriving (Show,Eq,Ord)

turns U = [L,R]
turns L = [U,D]
turns D = [L,R]
turns R = [U,D]

(x,y) >>> U = (x,y-1)
(x,y) >>> D = (x,y+1)
(x,y) >>> L = (x-1,y)
(x,y) >>> R = (x+1,y)

type State = (Int,Pos,Dir,[Pos]) -- (cost,pos,dir,previous positions)

neighbours :: State -> [State]
neighbours (cost,pos,dir,ps) = (cost+1,pos >>> dir, dir, pos:ps):[(cost+1000,pos,dir',ps) | dir' <- turns dir]

orderedUnion new old = foldr insert old (sort new)

dijkstra :: (Pos,Pos,Paths) -> [(Int,[Pos])]
dijkstra (start,end,paths) = go S.empty [(0,start,R,[])]
  where go :: S.Set (Pos,Dir) -> [State] -> [(Int,[Pos])]
        go visited (s@(cost,pos,dir,_):ss)
          | pos == end = [(cost,pos:ps) | (cost',pos',_,ps) <- s:ss, pos'==end, cost'==cost]
          | otherwise = go (S.insert (pos,dir) visited) (orderedUnion (nexts visited s) ss)
        nexts visited s = filter (possible visited) $ neighbours s
        possible visited (_,p,dir,_) = S.member p paths && not (S.member (p,dir) visited)

part1 = fst . head . dijkstra

part2 = S.size . S.fromList . concatMap snd . dijkstra

main = do
  --print . part1 =<< slurp
  print . part2 =<< slurp
