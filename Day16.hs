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

nexts paths (cost,pos,dir) = (cost+1,pos >>> dir, dir):[(cost+1000,pos,dir') | dir' <- turns dir]

orderedUnion new old = foldr insert old (sort new)

t s x = trace (s ++ ": " ++ show x) x

visualize :: Paths -> S.Set (Pos,Dir) -> String
visualize paths visited = unlines [ [ r (x,y) | x <- [0..maxX] ] | y <- [0..maxY] ]
  where maxX = maximum (map fst $ S.toList paths)
        maxY = maximum (map snd $ S.toList paths)
        r p | S.member p $ S.map fst visited = 'X'
            | S.member p paths = '.'
            | otherwise = ' '

dijkstra :: (Pos,Pos,Paths) -> Int
dijkstra (start,end,paths) = go S.empty [(0,start,R)]
  where go :: S.Set (Pos,Dir) -> [(Int,Pos,Dir)] -> Int
        go visited (s@(cost,pos,dir):ss)
          | pos == end = cost
          | S.member (pos,dir) visited = go visited ss
          | otherwise = --trace (visualize paths visited) $
                        --trace ("Q: "++show (s:ss)) $
                        go (S.insert (pos,dir) visited) (orderedUnion (filter (possible visited) $ nexts paths s) ss)
        possible visited (_,p,dir) = S.member p paths && not (S.member (p,dir) visited)
        same (_,p,dir) (_,p',dir') = p==p' && dir==dir'
part1 = dijkstra

main = do
  print . part1 =<< slurp
