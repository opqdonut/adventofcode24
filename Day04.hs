module Day04 where

import Data.List
import qualified Data.Map as M

slurp = lines <$> readFile "input-04"


count = length . filter ("XMAS"`isPrefixOf`) . tails
count2d = sum . map count

diagonals [[x]] = [[x]]
diagonals (xs:xss) = zipWith (:) heads $ [[]] ++ diagonals inner ++ [[]]
  where heads = reverse xs ++ map head xss
        inner = map tail xss

t1 = diagonals ["ABC","DEF","GHI"]
-- ABC       C
-- DEF ==>  B F
-- GHI     A E I
--          D H
--           G


-- 18 hits
example1 =
  ["MMMSXXMASM"
  ,"MSAMXMSMSA"
  ,"AMXSXMAAMM"
  ,"MSAMASMSMX"
  ,"XMASAMXAMM"
  ,"XXAMMXXAMA"
  ,"SMSMSASXSS"
  ,"SAXAMASAAA"
  ,"MAMMMXMMMM"
  ,"MXMXAXMASX"]

flop = map reverse
antidiagonals = diagonals . flop

part1 inp = sum . map count2d $
  [inp
  ,flop inp
  ,transpose inp
  ,flop $ transpose inp
  ,diagonals inp
  ,flop $ diagonals inp
  ,antidiagonals inp
  ,flop $ antidiagonals inp]

toMap inp = M.fromList [((x,y),c) | (y,row) <- zip [0..] inp, (x,c) <- zip [0..] row]

isXMas m (x,y) = m M.! (x,y) == 'A'
                 && and [M.member (x,y) m | x <- [x-1,x+1], y <- [y-1, y+1]]
                 && sort [m M.! (x-1,y-1), m M.! (x+1,y+1)] == "MS"
                 && sort [m M.! (x+1,y-1), m M.! (x-1,y+1)] == "MS"

part2 inp = length $ filter (isXMas m) $ M.keys m
  where m = toMap inp

main =
  --print . part1 =<< slurp
  print . part2 =<< slurp
