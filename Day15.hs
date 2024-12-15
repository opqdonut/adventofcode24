module Day15 where

import qualified Data.Map as M
import Data.List
import Data.Maybe
import qualified Data.Set as S

data Dir = U|L|D|R
  deriving (Show,Eq,Read)

data Thing = Wall | Box
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

visualize :: State -> String
visualize (State chart robot _) = unlines [ [r (x,y) | x <- [0..maxX]] | y <- [0..maxY] ]
  where ((maxX,maxY),_) = M.findMax chart
        r p
          | p == robot = '@'
          | otherwise = case M.lookup p chart
                        of Just Box -> 'O'
                           Just Wall -> '#'
                           Nothing -> '.'

(>>>) :: Pos -> Dir -> Pos
(x,y) >>> U = (x,y-1)
(x,y) >>> D = (x,y+1)
(x,y) >>> L = (x-1,y)
(x,y) >>> R = (x+1,y)

swapIn :: Pos -> Thing -> Chart -> (Chart,Maybe Thing)
swapIn p t c = (M.insert p t c, M.lookup p c)

push :: Pos -> Dir -> Chart -> Maybe Chart
push p d original = go remaining (p >>> d) (M.lookup p original)
  where remaining = M.delete p original
        go c _ Nothing = Just c
        go c _ (Just Wall) = Nothing
        go c p (Just Box) = let (c',thing) = swapIn p Box c
                            in go c' (p >>> d) thing

tick :: State -> State
tick (State chart robot (dir:moves)) =
  case push robot' dir chart
  of Just chart' -> State chart' robot' moves
     Nothing -> State chart robot moves
  where robot' = robot >>> dir

finish :: State -> State
finish s@(State _ _ []) = s
finish s = finish $ tick s

gps :: State -> Int
gps (State chart _ _) = sum [y*100+x | ((x,y),Box) <- M.toList chart]

part1 = gps . finish

lookupObstacle :: Chart -> Pos -> [Pos]
lookupObstacle c p = case (M.lookup p c, M.lookup (p>>>L) c)
                     of (Just Box, _) -> [p]
                        (Just Wall, _) -> [p]
                        (_, Just Box) -> [p>>>L]
                        _ -> []

bigBoxExtents p = [p, p>>>R]

deleteAll ks m = foldl' (flip M.delete) m ks

swapIn2 :: Pos -> Thing -> Chart -> (Chart,[Pos])
swapIn2 p Box c = (M.insert p Box $ deleteAll obsts c, obsts)
  where obsts = nub $ concatMap (lookupObstacle c) (bigBoxExtents p)

swapInAll :: [Pos] -> Chart -> (Chart,[Pos])
swapInAll [] c = (c,[])
swapInAll (p:ps) c = let (c',fromP) = swapIn2 p Box c
                         (c'',fromPs) = swapInAll ps c'
                     in (c'',fromP++fromPs)

push2 :: Pos -> Dir -> Chart -> Maybe Chart
push2 p d original
  | all (\p' -> M.lookup p' original /= Just Wall) initial = go remaining (map (>>>d) $ initial)
  | otherwise = Nothing
  where initial = lookupObstacle original p
        remaining = deleteAll initial original
        go c [] = Just c
        go c ps
          | all (\p' -> M.lookup p' c /= Just Wall) (concatMap bigBoxExtents ps) = let (c',new) = swapInAll ps c
                                                                                   in go c' (map (>>>d) new)
          | otherwise = Nothing

tick2 :: State -> State
tick2 (State chart robot (dir:moves)) =
  case push2 robot' dir chart
  of Just chart' -> State chart' robot' moves
     Nothing -> State chart robot moves
  where robot' = robot >>> dir

upscale :: State -> State
upscale (State chart (x,y) moves) = State chart' (x*2,y) moves
  where expand ((x,y),Wall) = [((2*x,y),Wall),((2*x+1,y),Wall)]
        expand ((x,y),Box) = [((2*x,y),Box)]
        chart' = M.fromList $ concatMap expand $ M.toList chart

finish2 :: State -> State
finish2 s@(State _ _ []) = s
finish2 s = finish2 $ tick2 s

part2 = gps . finish2 . upscale

visualize2 :: State -> String
visualize2 (State chart robot ms) = unlines $ moves:[ [r (x,y) | x <- [0..maxX]] | y <- [0..maxY] ]
  where moves = concatMap show (take 10 ms)
        ((maxX,maxY),_) = M.findMax chart
        r p
          | p == robot = '@'
          | otherwise = case (M.lookup p chart, M.lookup (p>>>L) chart)
                        of (Just Box,_) -> '['
                           (Just Wall,_) -> '#'
                           (_, Just Box) -> ']'
                           _ -> '.'

debug :: State -> IO ()
debug s = do
  putStrLn $ visualize2 s
  getLine
  debug (tick2 s)

play :: State -> IO ()
play s = do
  putStrLn $ visualize2 s
  dir <- readLn
  play (tick2 s {moves=[dir]})

main = do
  --print . part1 =<< slurp
  print . part2 =<< slurp
