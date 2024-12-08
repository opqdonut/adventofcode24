module Day08 where

import qualified Data.Map as M
import qualified Data.Set as S

type Pos = (Int,Int)

type Map = (Pos,M.Map Char [Pos])

parse :: String -> Map
parse s = ((maxX,maxY),M.fromListWith (++) [(c,[(x,y)]) | (y,row) <- rows
                                             , (x,c) <- zip [0..] row
                                             , c /= '.'])
  where rows = zip [0..] (lines s)
        maxY = fst (last rows)
        maxX = length (snd $ head rows) - 1

slurp = parse <$> readFile "input-08"

antinodes (x1,y1) (x2,y2) = [(x1-dx,y1-dy),(x2+dx,y2+dy)]
  where dx = x2-x1
        dy = y2-y1

between a b c = a <= b && b <= c

inBounds (maxX,maxY) (x,y) = between 0 x maxX && between 0 y maxY

allAntinodes :: Map -> S.Set Pos
allAntinodes (bounds,antennas) = S.fromList [p | ps <- M.elems antennas
                                               , p1 <- ps
                                               , p2 <- ps
                                               , p1 /= p2
                                               , p <- antinodes p1 p2
                                               , inBounds bounds p]

part1 = S.size . allAntinodes

example = parse "............\n\
                \........0...\n\
                \.....0......\n\
                \.......0....\n\
                \....0.......\n\
                \......A.....\n\
                \............\n\
                \............\n\
                \........A...\n\
                \.........A..\n\
                \............\n\
                \............\n"

visualize :: Map -> String
visualize m@((maxX,maxY),antennas) = unlines [ concat [[visAnt (x,y), visNode (x,y)] | x <- [0..maxX]]
                                             | y<-[0..maxY]]
  where an = allAntinodes m
        visNode p = if S.member p an then '#' else ' '
        visAnt p = case [sym | (sym,ps) <- M.assocs antennas
                             , p `elem` ps]
                   of (sym:_) -> sym
                      [] -> '.'

antinodes2 bounds (x1,y1) (x2,y2) = takeWhile (inBounds bounds) left ++ takeWhile (inBounds bounds) right
  where dx = x2-x1
        dy = y2-y1
        left = [(x1-k*dx,y1-k*dy) | k<-[0..]]
        right = [(x2+k*dx,y2+k*dy) | k<-[0..]]

allAntinodes2 (bounds,antennas) = S.fromList [p | ps <- M.elems antennas
                                                , p1 <- ps
                                                , p2 <- ps
                                                , p1 /= p2
                                                , p <- antinodes2 bounds p1 p2
                                                , inBounds bounds p]

part2 = S.size . allAntinodes2

main =
  --print . part1 =<< slurp
  print . part2 =<< slurp
