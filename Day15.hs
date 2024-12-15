module Day15 where

import qualified Data.Map as M
import Data.List
import qualified Data.Set as S

data Dir = U|L|D|R
  deriving (Show,Eq)

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

main = do
  print . part1 =<< slurp
