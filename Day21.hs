module Day21 where

import Data.List
import Data.Maybe
import Data.Ord

input = ["083A", "935A", "964A", "149A", "789A"]
example = ["029A", "980A", "179A", "456A", "379A"]

type Pad = [(Char,(Int,Int))]

pad :: [String] -> Pad
pad ls = [(c,(x,y)) | (y,l) <- zip [0..] ls
                    , (x,c) <- zip [0..] l]

numpad = pad ["789"
             ,"456"
             ,"123"
             ," 0A"]

arrowpad = pad [" ^A"
               ,"<v>"]

paths :: Pad -> Char -> Char -> [[Char]]
paths p from to
  | fromX == badX && toY == badY = [hori ++ vert ++ "A"]
  | fromY == badY && toX == badX = [vert ++ hori ++ "A"]
  | otherwise = nub [hori ++ vert ++ "A", vert ++ hori ++ "A"]
  where Just (badX,badY) = lookup ' ' p
        Just (fromX,fromY) = lookup from p
        Just (toX,toY) = lookup to p
        dX = toX-fromX
        dY = toY-fromY
        vert = replicate (abs dY) (if dY < 0 then '^' else 'v')
        hori = replicate (abs dX) (if dX<0 then '<' else '>')

pathsFor :: Pad -> [Char] -> [[Char]]
pathsFor p s = map concat $ sequence [paths p a b | (a:b:_) <- tails ('A':s)]

unlookup v kvs = lookup v (map (\(a,b)->(b,a)) kvs)

unbuttonsFor p ks = go (fromJust $ lookup 'A' p) ks
  where go (x,y) ('<':ks) = go (x-1,y) ks
        go (x,y) ('>':ks) = go (x+1,y) ks
        go (x,y) ('v':ks) = go (x,y+1) ks
        go (x,y) ('^':ks) = go (x,y-1) ks
        go (x,y) ('A':ks) = fromJust (unlookup (x,y) p) : go (x,y) ks
        go _ [] = []

pathsFull code = concatMap (pathsFor arrowpad) . concatMap (pathsFor arrowpad) $ pathsFor numpad code

bestPath = minimumBy (comparing length) . pathsFull

part1 inp = sum [length (bestPath code) * read (init code) | code <- inp]
