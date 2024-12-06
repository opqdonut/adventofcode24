module Day06 where

import qualified Data.Set as S

type Pos = (Int,Int)

data Dir = U | D | L | R
  deriving (Show,Eq,Ord)

data World = World {width :: Int, height :: Int, obstacles :: S.Set Pos}
  deriving Show

data Guard = Guard {position :: Pos, direction :: Dir}
  deriving (Show,Eq,Ord)

parse :: String -> (World,Guard)
parse s = (World w h obs, Guard pos U)
  where ls = lines s
        w = length (head ls)
        h = length ls
        points = [((x,y),c) | (y,row) <- zip [0..] ls, (x,c) <- zip [0..] row]
        obs = S.fromList [p | (p,'#') <- points]
        pos = head [p | (p,'^') <- points]

slurp = parse <$> readFile "input-06"

march :: Guard -> Guard
march (Guard (x,y) U) = Guard (x,y-1) U
march (Guard (x,y) D) = Guard (x,y+1) D
march (Guard (x,y) L) = Guard (x-1,y) L
march (Guard (x,y) R) = Guard (x+1,y) R

turnRight' U = R
turnRight' R = D
turnRight' D = L
turnRight' L = U

turnRight :: Guard -> Guard
turnRight (Guard p d) = Guard p (turnRight' d)

inbounds :: World -> Guard -> Bool
inbounds w g = 0 <= x && x < width w && 0 <= y && y < height w
  where (x,y) = position g

step :: World -> Guard -> Guard
step world guard
  | position next `S.member` obstacles world = turnRight guard
  | otherwise = next
  where next = march guard

visits :: World -> Guard -> S.Set Pos
visits world guard = S.fromList . map position . takeWhile (inbounds world) $ iterate (step world) guard

part1 :: (World,Guard) -> Int
part1 (world,guard) = S.size $ visits world guard

loops world init = go S.empty init
  where go acc g
          | g `S.member` acc = True
          | not (inbounds world g) = False
          | otherwise = go (S.insert g acc) (step world g)

newObstacle p w = w {obstacles = S.insert p (obstacles w)}

part2 :: (World,Guard) -> Int
part2 (world,init) = length (filter works tryObstacles)
  where tryObstacles = S.toList $ visits world init
        works obs = loops (newObstacle obs world) init

main =
  --print . part1 =<< slurp
  print . part2 =<< slurp

-- Took 8s when compiled with `ghc -O2 -main-is Day06 --make Day06.hs`
