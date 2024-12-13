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

-- want to solve ax*m+bx*n = px
-- for positive m,n

-- Solve the diofantean equation a*m+b*n=x
-- for positive m,n<=100. Return possible (m,n) pairs.
diofantean :: Int -> Int -> Int -> [(Int,Int)]
diofantean a b x =
  [(m,n) | m <- [0..100]
         , let (n,r) = divMod (x-m*a) b
         , r==0]

allRoutes (ax,ay) (bx,by) (gx,gy) =
  intersect (diofantean ax bx gx) (diofantean ay by gy)

solve1 :: (Pos,Pos,Pos) -> Int
solve1 (a,b,goal) = case allRoutes a b goal
                    of [] -> 0
                       xs -> minimum [a*3+b | (a,b) <- xs]

part1 = sum . map solve1

main = do
  print . part1 =<< slurp
