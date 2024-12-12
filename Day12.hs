module Day12 where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe

type Pos = (Int,Int)
type Chart = M.Map Pos Char
type Area = S.Set Pos

parse :: String -> Chart
parse s = M.fromList [((x,y),c) | (y,row) <- zip [0..] $ lines s
                                , (x,c) <- zip [0..] row]

slurp = parse <$> readFile "input-12"

get :: Chart -> Pos -> Char
get c p = fromMaybe '.' (M.lookup p c)

neighbours (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

extractArea :: Chart -> Pos -> Area
extractArea c (x,y) = go (S.singleton (x,y)) [(x,y)]
  where go area [] = area
        go area frontier = go (S.union area (S.fromList frontier')) frontier'
          where frontier' = [p | p <- nub $ concatMap neighbours frontier
                               , not (S.member p area)
                               , get c p == get c (x,y)]

-- part1 example == 1930
-- part2 example == 1206
example = parse "RRRRIICCFF\n\
                \RRRRIICCCF\n\
                \VVRRRCCFFF\n\
                \VVRCCCJFFF\n\
                \VVVVCJJCFE\n\
                \VVIVCCJJEE\n\
                \VVIIICJJEE\n\
                \MIIIIIJJEE\n\
                \MIIISIJEEE\n\
                \MMMISSJEEE\n"

allAreas :: Chart -> [Area]
allAreas c = go (M.keysSet c) []
  where go todo areas = case S.lookupMin todo of
          Nothing -> areas
          Just p -> let area = extractArea c p
                    in go (todo `S.difference` area) (area:areas)

perimeter :: Area -> Int
perimeter a = sum $ map per $ S.toList a
  where per p = length [ p' | p' <- neighbours p
                            , not (S.member p' a) ]

area :: Area -> Int
area = S.size

score a = perimeter a * area a

part1 = sum . map score . allAreas

spans [] = 0
spans [x] = 1
spans (x:y:xs)
  | x+1==y = spans (y:xs)
  | otherwise = 1 + spans (y:xs)

sidesAtX a minY maxY x0 = spans outs + spans ins
  where outs = [y | y<-[minY..maxY]
                  , not (S.member (x0-1,y) a)
                  , S.member (x0,y) a]
        ins = [y | y<-[minY..maxY]
                 , S.member (x0-1,y) a
                 , not (S.member (x0,y) a)]

sides a = sidesX + sidesY
  where xs = S.map fst a
        ys = S.map snd a
        (minX,maxX) = (S.findMin xs,S.findMax xs)
        (minY,maxY) = (S.findMin ys,S.findMax ys)
        flipped = S.map (\(x,y) -> (y,x)) a
        sidesX = sum [ sidesAtX a minY maxY x | x <- [minX..maxX+1] ]
        sidesY = sum [ sidesAtX flipped minX maxX y | y <- [minY..maxY+1] ]

-- part2 exampleE == 236
exampleE = parse "EEEEE\n\
                 \EXXXX\n\
                 \EEEEE\n\
                 \EXXXX\n\
                 \EEEEE\n"

score2 a = sides a * area a

part2 = sum . map score2 . allAreas

main = do
  --print . part1 =<< slurp
  print . part2 =<< slurp
