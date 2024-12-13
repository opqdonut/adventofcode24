module Day13 where

import Data.List
import Data.Maybe
import Control.Monad

type Pos = (Integer,Integer)

parseCoords :: String -> Pos
parseCoords c = read $ "(" ++ filter (`elem`"0123456789,") c ++ ")"

parse [] = []
parse ls = (parseCoords a, parseCoords b, parseCoords p):parse (dropWhile (=="") rest)
  where ([a,b,p],rest) = splitAt 3 ls

slurp = parse . lines <$> readFile "input-13"
example = parse . lines <$> readFile "example-13"

(a,b) +. (c,d) = (a+c,b+d)

minMaybe Nothing y = y
minMaybe x Nothing = x
minMaybe (Just x) (Just y) = Just (min x y)

allRoutes (ax,ay) (bx,by) (gx,gy) =
  [ (a,b) | a<-[0..min (div gx ax) (div gy ay)]
          , let (b,rx) = divMod (gx-a*ax) bx
          , rx == 0
          , let (b',ry) = divMod (gy-a*ay) by
          , ry == 0
          , b == b' ]

solve1 :: (Pos,Pos,Pos) -> Integer
solve1 (a,b,goal) = case allRoutes a b goal
                    of [] -> 0
                       xs -> minimum [a*3+b | (a,b) <- xs]

part1 = sum . map solve1

offset :: Pos
offset = (10000000000000,10000000000000)

-- no inputs are collinear! that means there is a unique solution
collinear (ax,ay) (bx,by) = ax*by==ay*bx

--     k*(ax,ay) + j*(bx,by) = (gx,gy)
-- <=> (gx,gy) - k*(ax,ay) = j*(bx,by)
-- <=> (gx-k*ax)/(gy-k*ay) = bx/by
-- <=> gx*by-k*ax*by = gy*bx-k*ay*bx
-- <=> k(ay*bx-ax*by) = gy*bx-gx*by
-- <=> k = (gy*bx-gx*by)/(ay*bx-ax*by)
-- and k needs to be an integer

divideEvenly x y = case divMod x y of (d,0) -> Just d
                                      _ -> Nothing

projectRoute (ax,ay) (bx,by) (gx,gy) = do
  a <- divideEvenly (gy*bx-gx*by) (ay*bx-ax*by)
  b <- divideEvenly (gx-a*ax) bx
  return (a,b)

solve2 :: (Pos,Pos,Pos) -> Integer
solve2 (a,b,goal) = case projectRoute a b goal
                    of Nothing -> 0
                       Just (a,b) -> a*3+b

addOffset (a,b,g) = (a,b,g +. offset)

part2 = sum . map (solve2 . addOffset)

main = do
  --print . part1 =<< slurp
  print . part2 =<< slurp
