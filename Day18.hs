module Day18 where

import Data.Maybe
import Data.List
import qualified Data.Set as S

type Pos = (Int,Int)
type Chart = S.Set Pos

parse :: String -> [Pos]
parse = map p . lines
  where p s = read ("("++s++")")

slurp = parse <$> readFile "input-18"
example = parse <$> readFile "example-18"

neighbours (x,y) = [(x-1,y)
                   ,(x+1,y)
                   ,(x,y-1)
                   ,(x,y+1)]

bfs :: Pos -> Pos -> Chart -> Maybe (Int,[Pos])
bfs start end chart = go S.empty [(0,[start])]
  where go visited ((d,path@(p:_)):qs)
          | p==end = Just (d,path)
          | S.member p visited = go visited qs
          | otherwise = go (S.insert p visited) (qs ++ [(d+1,n:path) | n <- neighbours p
                                                                       , S.member n chart
                                                                       , not (S.member n visited)])
        go _ [] = Nothing

allPositions = S.fromList [(x,y) | x <- [0..70], y <- [0..70]]

part1 inp = fst $ fromJust $ bfs (0,0) (70,70) $ S.difference allPositions $ S.fromList $ take 1024 inp

part2 inp = go allPositions initialPath inp
  where mkChart cs = S.difference allPositions $ S.fromList cs
        Just (_,initialPath) = bfs (0,0) (70,70) allPositions
        go chart path (i:inp)
          | notElem i path = go chart' path inp
          | otherwise = case bfs (0,0) (70,70) chart'
                        of Just (_,path') -> go chart' path' inp
                           Nothing -> i
          where chart' = S.delete i chart

main = do
  --print . part1 =<< slurp
  print . part2 =<< slurp


--30,10
