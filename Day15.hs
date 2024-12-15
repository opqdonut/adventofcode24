module Day15 where

import qualified Data.Map as M
import Data.List
import Data.Maybe
import qualified Data.Set as S

data Dir = U|L|D|R
  deriving (Show,Eq,Read)

data Thing = Wall | Box | BigBox
  deriving (Show,Eq)

type Pos = (Int,Int)
type Chart = M.Map Pos Thing

data State = State {chart :: Chart, robot :: Pos, moves :: [Dir]}
  deriving Show

parse :: String -> State
parse s = State chart robot moves
  where ls = lines s
        (chartLines,"":moveLines) = break (=="") ls
        coords = [((x,y),c) | (y,line) <- zip [0..] chartLines
                            , (x,c) <- zip [0..] line]
        robot = head [pos | (pos,'@') <- coords]
        chart = M.fromList [(pos,case c of '#' -> Wall; 'O' -> Box) | (pos,c) <- coords, c /= '@', c /= '.']
        parseMove 'v' = D
        parseMove '^' = U
        parseMove '<' = L
        parseMove '>' = R
        moves = map parseMove $ concat moveLines

example1 = parse <$> readFile "example1-15"
example2 = parse <$> readFile "example2-15"
example3 = parse <$> readFile "example3-15"
slurp = parse <$> readFile "input-15"

(>>>) :: Pos -> Dir -> Pos
(x,y) >>> U = (x,y-1)
(x,y) >>> D = (x,y+1)
(x,y) >>> L = (x-1,y)
(x,y) >>> R = (x+1,y)

gps :: State -> Int
gps (State chart _ _) = sum [y*100+x | ((x,y),t) <- M.toList chart, t /= Wall]

part1 = gps . finish

lookupObstacle :: Chart -> Pos -> [(Thing,Pos)]
lookupObstacle c p = case (M.lookup p c, M.lookup (p>>>L) c)
                     of (Just t, _) -> [(t,p)]
                        (_, Just BigBox) -> [(BigBox,p>>>L)]
                        _ -> []

extents BigBox p = [p, p>>>R]
extents _ p = [p]

deleteAll ks m = foldl' (flip M.delete) m ks

pickUp :: Chart -> Pos -> (Chart,[(Thing,Pos)])
pickUp c p = (deleteAll (map snd obsts) c, obsts)
  where obsts = lookupObstacle c p

swapIn :: Pos -> Thing -> Chart -> (Chart,[(Thing,Pos)])
swapIn p thing c = (M.insert p thing c', concat tps)
  where (c',tps) = mapAccumL pickUp c (extents thing p)

swapInAll :: [(Thing,Pos)] -> Chart -> (Chart,[(Thing,Pos)])
swapInAll tps c = (c',concat tps')
  where (c',tps') = mapAccumL (\s (t,p) -> swapIn p t s) c tps

push :: Pos -> Dir -> Chart -> Maybe Chart
push p d original = go remaining initial
  where (remaining,initial) = pickUp original p
        go :: Chart -> [(Thing,Pos)] -> Maybe Chart
        go c [] = Just c
        go c tps
          | all (\(t,_) -> t/=Wall) tps = let moved = map (\(t,p) -> (t,p>>>d)) tps
                                              (c',new) = swapInAll moved c
                                          in go c' new
          | otherwise = Nothing

tick :: State -> State
tick (State chart robot (dir:moves)) =
  case push robot' dir chart
  of Just chart' -> State chart' robot' moves
     Nothing -> State chart robot moves
  where robot' = robot >>> dir

upscale :: State -> State
upscale (State chart (x,y) moves) = State chart' (x*2,y) moves
  where expand ((x,y),Wall) = [((2*x,y),Wall),((2*x+1,y),Wall)]
        expand ((x,y),Box) = [((2*x,y),BigBox)]
        chart' = M.fromList $ concatMap expand $ M.toList chart

finish :: State -> State
finish s@(State _ _ []) = s
finish s = finish $ tick s

part2 = gps . finish . upscale

visualize :: State -> String
visualize (State chart robot ms) = unlines $ moves:[ [r (x,y) | x <- [0..maxX]] | y <- [0..maxY] ]
  where moves = concatMap show (take 10 ms)
        ((maxX,maxY),_) = M.findMax chart
        r p
          | p == robot = '@'
          | otherwise = case (M.lookup p chart, M.lookup (p>>>L) chart)
                        of (Just BigBox,_) -> '['
                           (Just Wall,_) -> '#'
                           (Just Box,_) -> 'O'
                           (_, Just BigBox) -> ']'
                           _ -> '.'

debug :: State -> IO ()
debug s = do
  putStrLn $ visualize s
  getLine
  debug (tick s)

play :: State -> IO ()
play s = do
  putStrLn $ visualize s
  dir <- readLn
  play (tick s {moves=[dir]})

main = do
  print . part1 =<< slurp
  print . part2 =<< slurp
