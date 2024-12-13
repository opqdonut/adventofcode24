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

-- 3 A, 1 B

(a,b) +. (c,d) = (a+c,b+d)

(a,b) <. (c,d) = a<c && b<d

minMaybe Nothing y = y
minMaybe x Nothing = x
minMaybe (Just x) (Just y) = Just (min x y)

solve1 :: (Pos,Pos,Pos) -> Integer
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

-- we know that m*a+n*b has only solutions of the form k*(gcd a b)
soluble ((ax,ay),(bx,by),(gx,gy)) = mod gx (gcd ax bx) == 0 && mod gy (gcd ay by) == 0

-- no inputs are collinear! that means there is a unique solution
collinear (ax,ay) (bx,by) = ax*by==ay*bx

aRoute (ax,ay) (bx,by) (gx,gy) = go 0
    where go a = case (divMod (gx-a*ax) bx, divMod (gy-a*ay) by)
                 of ((b,0),(b',0)) | b==b' -> (a,b)
                    _ -> go (a+1)


-- k*(ax,ay) + j*(bx,by) = (gx,gy)
-- eli (gx,gy) - k*(ax,ay) = j*(bx,by)
-- eli (gx-k*ax)/(gy-k*ay) = bx/by
-- eli gx*by-k*ax*by = gy*bx-k*ay*bx
-- eli k(ay*bx-ax*by) = gy*bx-gx*by
-- eli k = (gy*bx-gx*by)/(ay*bx-ax*by)
projectRoute (ax,ay) (bx,by) (gx,gy) = case divMod (gy*bx-gx*by) (ay*bx-ax*by)
                                       of (a,0) -> Just (a, div (gx-a*ax) bx)
                                          _ -> Nothing

-- hack: when bx==by, projectRoute returns a false solution.
-- check that flipping the arguments gives the same solution.
projectRoute' a b g = do
  (ka,kb) <- projectRoute a b g
  (kb',ka') <- projectRoute b a g
  guard (ka==ka')
  guard (kb==kb')
  return (ka,kb)

doubleCheck (ax,ay) (bx,by) (gx,gy) = case projectRoute (ax,ay) (bx,by) (gx,gy)
                                      of Just (a,b) -> Just (ck a b, a, b)
                                         Nothing -> Nothing
    where ck a b = a*ax + b*bx == gx && a*ay + b*by == gy

solve2 :: (Pos,Pos,Pos) -> Integer
solve2 (a,b,goal) = case projectRoute' a b goal
                    of Nothing -> 0
                       Just (a,b) -> a*3+b

addOffset (a,b,g) = (a,b,g +. offset)

part2 = sum . map (solve2 . addOffset)

main = do
  --print . part1 =<< slurp
  print . part2 =<< slurp

--(Just (False,48,129870129966),((74,12),(77,77),(10000000010944,10000000007968)))
--(Just (False,81,344827586215),((81,11),(29,29),(10000000006822,10000000001152)))
