module Day21 where

import Data.List
import Data.Maybe
import Data.Ord
import Data.Array
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M

input = ["083A", "935A", "964A", "149A", "789A"]
example = ["029A", "980A", "179A", "456A", "379A"]

type Pad = [(Char,(Int,Int))]

pad :: [String] -> Pad
pad ls = [(c,(x,y)) | (y,l) <- zip [0..] ls
                    , (x,c) <- zip [0..] l]

numpad = pad ["789"
             ,"456"
             ,"123"
             ," 0A"]

arrowpad = pad [" ^A"
               ,"<v>"]

paths :: Pad -> Char -> Char -> [[Char]]
paths p from to
  | fromX == badX && toY == badY = [hori ++ vert ++ "A"]
  | fromY == badY && toX == badX = [vert ++ hori ++ "A"]
  | otherwise = nub [hori ++ vert ++ "A", vert ++ hori ++ "A"]
  where Just (badX,badY) = lookup ' ' p
        Just (fromX,fromY) = lookup from p
        Just (toX,toY) = lookup to p
        dX = toX-fromX
        dY = toY-fromY
        vert = replicate (abs dY) (if dY < 0 then '^' else 'v')
        hori = replicate (abs dX) (if dX<0 then '<' else '>')

pathsFor :: Pad -> [Char] -> [[Char]]
pathsFor p s = map concat $ sequence [paths p a b | (a:b:_) <- tails ('A':s)]

memoizedShortestArrowPadPath :: Char -> Char -> [Char]
memoizedShortestArrowPadPath = \f t -> m M.! (f,t)
  where m = M.fromList [((a,b),minimumBy (comparing length) (paths arrowpad a b)) | a <- "<>^vA", b <- "<>^vA"]

greedyNumpadPath :: Pad -> [Char] -> [Char]
greedyNumpadPath p s = concat $ [memoizedShortestArrowPadPath a b | (a:b:_) <- tails ('A':s)]

unlookup v kvs = lookup v (map (\(a,b)->(b,a)) kvs)

unbuttonsFor p ks = go (fromJust $ lookup 'A' p) ks
  where go (x,y) ('<':ks) = go (x-1,y) ks
        go (x,y) ('>':ks) = go (x+1,y) ks
        go (x,y) ('v':ks) = go (x,y+1) ks
        go (x,y) ('^':ks) = go (x,y-1) ks
        go (x,y) ('A':ks) = fromJust (unlookup (x,y) p) : go (x,y) ks
        go _ [] = []

pathsFull code = concatMap (pathsFor arrowpad) . concatMap (pathsFor arrowpad) $ pathsFor numpad code

allShortest ss = [s | s <- ss, length s == m]
  where m = minimum (map length ss)

bestPath = head . allShortest . pathsFull

bestPath' code = head . allShortest . concatMap (pathsFor arrowpad) . allShortest . concatMap (pathsFor arrowpad) . allShortest $ pathsFor numpad code

part1 inp = sum [length (bestPath code) * read (init code) | code <- inp]

frequencies xs = [(length x, head x) | x <- group xs]

shortestPaths2 0 code = (pathsFor numpad code)
shortestPaths2 n code = map (greedyNumpadPath arrowpad) $ shortestPaths2 (n-1) code

sP2 n code = minimum $ map length $ shortestPaths2 n code

--part2 inp = sum [sP2 25 code * read (init code) | code <- inp]

--                            <<^^A
--                         v<<AA>^AA>A
-- v<<A>>^AvA^A     <vA<AA>>^AAvA<^A>AAvA^A <vA>^AA<A>Av<<A>A>^AAAvA<^A>A
-- v<<A>>^AvA^A v<<A>>^AA<vA<A>>^AAvAA<^A>A <vA>^AA<A>Av<<A>A>^AAAvA<^A>A
--                         <AAv<AA>>^A
--                            ^^<<A


------- fresh start

type Memo = M.Map (Int,Char,Char) Int

memo :: Ord a => (a -> b) -> a -> State (M.Map a b) b
memo f args = do
  old <- gets (M.lookup args)
  case old of Just x -> return x
              Nothing -> let x = f args
                         in modify' (M.insert args x) >> return x

sumSteps f pth = sum <$> zipWithM f ('A':pth) pth

search' :: Int -> Char -> Char -> State Memo Int
search' limit from to = minimum <$> mapM (sumSteps (mgogo limit)) (paths numpad from to)
  where gogo :: Int -> Char -> Char -> State Memo Int
        gogo 0 f t = return 1
        gogo n f t = minimum <$> mapM (sumSteps (mgogo (n-1))) (paths arrowpad f t)
        mgogo n f t = do old <- gets (M.lookup (n,f,t))
                         case old of Just x -> return x
                                     Nothing -> do x <- gogo n f t
                                                   modify' (M.insert (n,f,t) x)
                                                   return x

search limit code = evalState (sumSteps (search' limit) code) M.empty

part2 inp = sum [search 25 code * read (init code) | code <- inp]
