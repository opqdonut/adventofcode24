module Day05 where

import qualified Data.Set as S
import Data.List

parse :: String -> (S.Set (Int,Int),[[Int]])
parse s = (S.fromList pairs,pages)
  where ls = lines s
        (rules,_:updates) = break (=="") ls
        pairs = [(read a, read b) | r <- rules, let (a,_:b) = break (=='|') r]
        pages = [read ("["++u++"]") | u <- updates]

slurp = parse <$> readFile "input-05"

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = map ((,)x) xs ++ pairs xs

inOrder rules ps = all (\(a,b) -> not $ (b,a) `S.member` rules) $ pairs ps

middlePage ps = ps !! (length ps `div` 2)

part1 (rules,pages) =
  sum .
  map middlePage .
  filter (inOrder rules) $
  pages

order rules ps = sortBy cmp ps
  where cmp a b
          | a==b = EQ
          | (a,b) `S.member` rules = LT
          | otherwise = GT

part2 (rules,pages) =
  sum .
  map middlePage .
  map (order rules) .
  filter (not . inOrder rules) $
  pages

main =
  --print . part1 =<< slurp
  print . part2 =<< slurp
