module Day20 where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

type Pos = (Int,Int)

parse :: String -> (Pos,Pos,S.Set Pos)
parse s = (start,end,S.fromList paths)
  where coords = [((x,y),c) | (y,line) <- zip [0..] (lines s)
                            , (x,c) <- zip [0..] line]
        start = head [pos | (pos,'S') <- coords]
        end = head [pos | (pos,'E') <- coords]
        paths = [(x,y) | ((x,y),c) <- coords, c /= '#']

neighbours len (x0,y0) = [(x,y) | x <- [x0-len..x0+len]
                                , let ylen = len - abs (x-x0)
                                , y <- [y0-ylen..y0+ylen]]

distance (x0,y0) (x1,y1) = abs (x0-x1) + abs (y0-y1)

solve :: (Pos,Pos,S.Set Pos) -> M.Map Pos Int
solve (start,end,paths) = go M.empty [(0,start)]
  where go :: M.Map Pos Int -> [(Int,Pos)] -> M.Map Pos Int
        go dists ((dist,p):ps)
          | p==end = M.insert p dist dists
          | M.member p dists = go dists ps
          | otherwise = go (M.insert p dist dists) (ps ++ [(dist+1,n) | n <- neighbours 1 p
                                                                      , S.member n paths
                                                                      , not (M.member n dists)])

cheats :: Int -> M.Map Pos Int -> [(Int,Pos,Pos)]
cheats len dists = concatMap cheat $ M.assocs dists
  where cheat (p,d) = [ (win,p,n) | n <- neighbours len p
                                  , Just nd <- [M.lookup n dists]
                                  , let win = nd - d - distance p n
                                  , win > 0]

example = parse <$> readFile "example-20"
slurp = parse <$> readFile "input-20"

part1 i = length [d | (d,_,_) <- cheats 2 (solve i)
                    , d>=100]

part2 i = length [d | (d,_,_) <- cheats 20 (solve i)
                   , d>=100]


main = do
  --print . part1 =<< slurp
  print . part2 =<< slurp
