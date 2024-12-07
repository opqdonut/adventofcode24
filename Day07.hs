module Day07 where

parseLine :: String -> (Int,[Int])
parseLine s = (read res,map read vals)
  where (res,':':rest) = break (==':') s
        vals = words rest

slurp = map parseLine . lines <$> readFile "input-07"

possibleValues ops (n:nums) = foldl tryAll [n] nums
  where tryAll acc x = [o y x | o <- ops, y <- acc]

possible ops (res,vals) = res `elem` possibleValues ops vals

part1 inp = sum . map fst $ filter (possible [(+),(*)]) inp

a ||| b = read (show a ++ show b)

part2 inp = sum . map fst $ filter (possible [(+),(*),(|||)]) inp

main =
  --print . part1 =<< slurp
  print . part2 =<< slurp
