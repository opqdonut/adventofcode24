module Day23 where

import Data.List
import qualified Data.Map as M

type Graph = M.Map String [String]

parse i = M.fromListWith (++) [e | (u,v) <- edges, e <- [(u,[v]),(v,[u])]]
  where edges = map p . lines $ i
        p s = let (a,'-':b) = break (=='-') s in (a,b)

slurp = parse <$> readFile "input.23"
example = parse <$> readFile "example.23"

triangles g = [ [u,v,w] | (u,vs) <- M.assocs g, v <- vs, u<v, w <- g M.! v, v<w, u `elem` (g M.! w)]

part1 g = length [t | t <- triangles g, any ("t" `isPrefixOf`) t]

grow g clique = map (:clique) candidates
  where candidates = foldr1 intersect $ map (filter lt . (g M.!)) clique
        lt x = x < head clique

maxCliques g = last $ takeWhile (not.null) $ iterate (concatMap (grow g)) (triangles g)

part2 g = filter (not.(`elem`"[]\"")) $ show $ head $ maxCliques g

main = do
  --print . part1 =<< slurp
  putStrLn . part2 =<< slurp
