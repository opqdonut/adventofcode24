module Day21 where

import Data.List
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

part lim inp = sum [search lim code * read (init code) | code <- inp]

part1 = part 2
part2 = part 25
