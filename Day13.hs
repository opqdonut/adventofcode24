module Day13 where

import Data.List
import Data.Maybe

type Pos = (Int,Int)

parseCoords :: String -> Pos
parseCoords c = read $ "(" ++ filter (`elem`"0123456789,") c ++ ")"

parse [] = []
parse ls = (parseCoords a, parseCoords b, parseCoords p):parse (dropWhile (=="") rest)
  where ([a,b,p],rest) = splitAt 3 ls

slurp = parse . lines <$> readFile "input-13"
example = parse . lines <$> readFile "example-13"

-- 3 A, 1 B

(a,b) +. (c,d) = (a+c,b+d)

(a,b) <. (c,d) = a<c && b<d

minMaybe Nothing y = y
minMaybe x Nothing = x
minMaybe (Just x) (Just y) = Just (min x y)

solve1 :: (Pos,Pos,Pos) -> Int
solve1 (a,b,goal) = case allRoutes2 a b goal
                    of [] -> 0
                       xs -> minimum [a*3+b | (a,b) <- xs]

part1 = sum . map solve1

offset :: Pos
offset = (10000000000000,10000000000000)

allRoutes2 (ax,ay) (bx,by) (gx,gy) =
  [ (a,b) | a<-[0..min (div gx ax) (div gy ay)]
          , let (b,rx) = divMod (gx-a*ax) bx
          , rx == 0
          , let (b',ry) = divMod (gy-a*ay) by
          , ry == 0
          , b == b' ]


solve2 :: (Pos,Pos,Pos) -> Int
solve2 (a,b,goal) = case allRoutes2 a b (goal +. offset)
                    of [] -> 0
                       xs -> minimum [a*3+b | (a,b) <- xs]

part2 = sum . map solve2

main = do
  print . part1 =<< slurp
