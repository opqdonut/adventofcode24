module Day25 where

import Data.List

splitBlocks :: String -> [[String]]
splitBlocks s = unfoldr chop (lines s)
  where chop [] = Nothing
        chop ls = let (a,rest) = break (=="") ls in Just (a,drop 1 rest)

data Input = Lock [Int] | Key [Int]
  deriving (Show,Ord,Eq)

parseBlock :: [String] -> Input
parseBlock ss = case head ss
                of "#####" -> Lock is
                   _ -> Key is
  where is = map (length . filter (=='#')) $ transpose ss

parse = map parseBlock . splitBlocks

input = parse <$> readFile "input-25"
example = parse <$> readFile "example-25"

height = 7

fits (Key ks) (Lock ls) = all (<=height) $ zipWith (+) ks ls

part1 is = length [(k,l) | k@(Key _) <- is, l@(Lock _) <- is, fits k l]

main = do
  print . part1 =<< input
