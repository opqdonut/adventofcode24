module Day09 where

import Data.Char (digitToInt)
import Data.List
import Data.Maybe
import qualified Data.IntMap as M
import qualified Data.Set as S

type Disk = M.IntMap (Int,Maybe Int)

parse :: String -> Disk
parse s = file 0 0 M.empty (map digitToInt s)
  where file  pos fileno acc (siz:rest) = empty (pos+siz) (fileno+1) (M.insert pos (siz,Just fileno) acc) rest
        file  _   _      acc [] = acc
        empty pos fileno acc (siz:rest) = file (pos+siz) fileno (M.insert pos (siz,Nothing) acc) rest
        empty _   _      acc [] = acc

slurp = parse <$> init <$> readFile "input-09"

example1 = parse "90909"
example2 = parse "2333133121414131402"

render d = concatMap r (M.elems d)
  where r (siz,Nothing) = replicate siz '.'
        r (siz,Just i) = replicate siz (head $ show i)

type Blocks = M.IntMap Int

blocks :: Disk -> Blocks
blocks d = M.fromList [ (i,fileno) | (pos,(siz,Just fileno)) <- M.assocs d
                                   , i <- [pos..pos+siz-1] ]

packBlocks :: Blocks -> Blocks
packBlocks d = go 0 d
  where go i d
          | i > end = d
          | i `M.member` d = go (i+1) d
          | otherwise = go (i+1) (M.insert i lst $ M.deleteMax d)
          where (end,lst) = M.findMax d

cksum :: Blocks -> Integer
cksum d = sum $ map (\(x,y) -> fromIntegral x * fromIntegral y) $ M.assocs d

part1 = cksum . packBlocks . blocks

findSpace :: Int -> Int -> Disk -> Maybe Int
findSpace before wanted d = listToMaybe [ pos | (pos,(siz,Nothing)) <- M.assocs d
                                              , siz >= wanted
                                              , pos < before ]

moveFile from to d
  | sizPlace == sizFile = fileMoved
  | sizPlace > sizFile = M.insert (to+sizFile) (sizPlace-sizFile,Nothing) fileMoved
  where (sizPlace,Nothing) = d M.! to
        (sizFile,Just fileno) = d M.! from
        fileMoved = M.insert to (sizFile,Just fileno) $ M.delete from $ d

packFiles :: Disk -> Disk
packFiles d = foldl' packFile d files
  where files = reverse [ f | f@(pos,(siz,Just fileno)) <- M.assocs d ]
        packFile d (pos,(siz,Just fileno)) = case findSpace pos siz d of
                                               Nothing -> d
                                               Just pos' -> moveFile pos pos' d


part2 = cksum . blocks . packFiles

main = do
  --print . part1 =<< slurp
  --print $ render $ example2
  --print $ render $ packFiles $ example2
  --print $ cksum $ blocks $ packFiles $ example2
  print . part2 =<< slurp
