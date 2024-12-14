module Day14 where

import Data.List
import qualified Data.Set as S
import Control.Monad

type Pos = (Int,Int)
data Robot = Robot {pos :: Pos, vel :: Pos}
  deriving (Show, Eq, Ord)

parseRobot l = Robot (read $ "(" ++ spos ++ ")") (read $ "(" ++ svel ++ ")")
  where [wpos,wvel] = words l
        Just spos = stripPrefix "p=" wpos
        Just svel = stripPrefix "v=" wvel

slurp = map parseRobot . lines <$> readFile "input-14"

(a,b) <+> (c,d) = (a+c,b+d)

wrap (w,h) (x,y) = (mod x w, mod y h)

tick :: (Int,Int) -> Robot -> Robot
tick (w,h) (Robot p v) = Robot (wrap (w,h) $ p <+> v) v

quadrant :: (Int,Int) -> Pos -> Int
quadrant (w,h) (x,y)
  | x < xmid && y < ymid = 1
  | x < xmid && y > ymid = 2
  | x > xmid && y < ymid = 3
  | x > xmid && y > ymid = 4
  | otherwise = 0
    where xmid = div w 2
          ymid = div h 2

bigLimits = (101,103)

safetyScore :: (Int,Int) -> [Robot] -> Int
safetyScore limits rs = product [ length [ r | r <- rs, quadrant limits (pos r) == q ]
                                | q <- [1..4] ]

part1 inp = safetyScore bigLimits $ iterate (map (tick bigLimits)) inp !! 100

heuristic1 rs = abs (left-right)
  where left = length $ filter ((`elem` [1,2]).quadrant bigLimits.pos) rs
        right = length $ filter ((`elem` [3,4]).quadrant bigLimits.pos) rs

heuristic2 rs = length rs - S.size (S.fromList (map pos rs))

heuristic = heuristic2

visualize :: (Int,Int) -> [Robot] -> IO ()
visualize (w,h) rs = putStrLn $ unlines ls
  where coords = S.fromList (map pos rs)
        ls = [ [ if S.member (x,y) coords then '#' else '.' | x <- [0..w-1] ] | y<-[0..h-1] ]

part2 inp = forM_ (take 100 minimums) $ \(i,rs) -> do
  putStrLn "--"
  visualize bigLimits rs
  print (heuristic rs)
  print i
  where evolution = zip [0..] $ iterate (map (tick bigLimits)) inp
        minimums = go (length inp) evolution
        go best ((i,r):rest)
          | heuristic r < best = (i,r):go (heuristic r) rest
          | otherwise = go best rest

main = do
  --print . part1 =<< slurp
  part2 =<< slurp
