module Day24 where

import Data.List
import qualified Data.Map as M
import Data.Bits

parseSetting s = (pin,read val)
  where (pin,':':' ':val) = break (==':') s

data Op = AND | OR | XOR
  deriving (Show,Eq,Ord,Read)

op AND = (.&.)
op OR = (.|.)
op XOR = xor


parseGate s = (out,(read op,p1,p2))
  where [p1,op,p2,"->",out] = words s

type State = M.Map String Int
type Ops = M.Map String (Op,String,String)

parse :: String -> (State, Ops)
parse s = (M.fromList $ map parseSetting ss, M.fromList $ map parseGate ops)
  where ls = lines s
        (ss,"":ops) = break (=="") ls

input = parse <$> readFile "input.24"
example = parse <$> readFile "example.24"

eval :: Ops -> String -> State -> State
eval ops pin state
  | M.member pin state = state
  | otherwise = let (o,a,b) = ops M.! pin
                    state' = eval ops a state
                    state'' = eval ops b state'
                in M.insert pin (op o (state'' M.! a) (state'' M.! b)) state''

evalAll :: Ops -> State -> State
evalAll ops state = foldl' (flip (eval ops)) state (M.keys ops)

fromBits bs = foldr (\b acc -> b + 2 * acc) 0 bs

outputPins state = [(p,v) | (p@('z':_),v) <- M.assocs state]
extractOutput state = [v | ('z':_,v) <- M.assocs state]

part1 :: (State,Ops) -> Int
part1 (state,ops) = fromBits $ extractOutput final
  where final = evalAll ops state

main = do
  print . part1 =<< input

toBits 0 = []
toBits i = let (i',b) = i `divMod` 2 in b : toBits i'

nBits :: Int
nBits = 44

evalOn :: Ops -> [Int] -> [Int] -> State
evalOn ops x y = evalAll ops state
  where s d = case show d of [c] -> ['0',c]; cs -> cs
        state = M.fromList (zipWith (\i v -> ('x':s i, v)) [0..nBits] (x ++ repeat 0)
                            ++ zipWith (\i v -> ('y':s i, v)) [0..nBits] (y ++ repeat 0))

opsAsFunction :: Ops -> [Int] -> [Int] -> [Int]
opsAsFunction ops x y = extractOutput $ evalOn ops x y

candidates ops x y = [p | ((p,a),b) <- zip (outputPins actual) expected, a/=b]
  where expected = toBits (fromBits x + fromBits y)
        actual = evalOn ops x y

allDeps ops pin = case M.lookup pin ops of
                    Nothing -> []
                    Just (_,a,b) -> pin : concatMap (allDeps ops) [a,b]

allDeps' ops pin = pin : case M.lookup pin ops of
                           Nothing -> []
                           Just (_,a,b) -> concatMap (allDeps' ops) [a,b]

-- (_,ops) <- input
-- cs = candidates ops [0] (replicate nBits 1)
-- nub $ concatMap (allDeps ops) cs

swap :: String -> String -> Ops -> Ops
swap a b ops = M.insert a (ops M.! b) $ M.insert b (ops M.! a) $ ops

findError :: Ops -> ([Int],[Int])
findError ops = head [ (x,y) | n <- [1..nBits], let x = replicate n 1, let y = [1], opsAsFunction ops x y /= take (nBits+2) (replicate n 0 ++ [1] ++ repeat 0) ]

-- findError ops
--   => ([1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],[1])
-- candidates ops [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1] [1]
--   => "z15"
-- length $ allDeps ops "z15"
--   => 61

canSwap :: Ops -> String -> String -> Bool
canSwap ops a b = not (a `elem` allDeps ops b) && not (b `elem` allDeps ops a)

allSwaps :: Ops-> [String] -> [(String,String)]
allSwaps ops pins = [(a,b) | (a:bs) <- tails pins, b <- bs, canSwap ops a b]

-- length $ allSwaps ops (allDeps ops "z15")
--   => 900

helps :: Ops -> ([Int],[Int]) -> [(String,String)] -> [(String,String)]
helps ops (x,y) swaps = [(a,b) | (a,b) <- swaps, candidates (swap a b ops) x y == []]

-- helps ops ([1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],[1]) (allSwaps ops (allDeps ops "z15"))
--  => [("ctg","mrm"),("dqg","mrm")]

-- *Day24> candidates (swap "ctg" "mrm" ops) [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1] [1]
-- ["z15"]
-- *Day24> candidates (swap "dqg" "mrm" ops) [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1] [1]
-- ["z15"]

-- *Day24> let ops1 = swap "ctg" "mrm" ops
-- *Day24> helps ops1 ([1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],[1]) (allSwaps ops1 (allDeps ops1 "z15"))
-- []
-- *Day24> let ops2 = swap "dqg" "mrm" ops
-- *Day24> helps ops2 ([1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],[1]) (allSwaps ops2 (allDeps ops2 "z15"))
-- []

-- try to discover irregularities in the network
traces ops = go [] (map fst (outputPins ops))
  where go seen [] = [("unconnected",M.keys ops \\ seen)]
        go seen (p:pins) = let d = nub (allDeps' ops p) in (p,(d \\ seen)) : go (union d seen) pins

-- "z20" needs to be swapped with something

-- *Day24> helps ops ((replicate 20 0 ++ [1]),[]) swaps
-- [("z20","hbp"),("z20","msn"),("z20","cqr")]
-- *Day24> helps ops ((replicate 20 0 ++ [1]),(replicate 20 0 ++ [1])) swaps
-- [("z20","hbp"),("z20","fkg"),("z20","wrb"),("z20","mjm"),("z20","vct"),("z20","msn"),("z20","cqr")]
-- *Day24> helps ops ((replicate 19 0 ++ [1,1]),(replicate 19 0 ++ [1])) swaps
-- [("z20","mjm"),("z20","cqr")]
--
-- looks like ("z20","cqr") is a winner!

getsWrong ops = [i | i <- [0..nBits], let x = replicate (i-1) 0 ++ [1], candidates ops x x /= []]

-- *Day24> (_,ops) <- input
-- *Day24> let ops' = swap "z20" "cqr" ops
-- *Day24> getsWrong ops
-- [15,16,20,21,28,37]
-- *Day24> getsWrong ops'
-- [15,16,28,37]

-- *Day24> lookup "z15" $ traces ops'
-- Just ["z15","dnn","rjm","x15","y15","ctg","dqg","gnc","mrm"]
-- *Day24> lookup "z16" $ traces ops'
-- Just ["z16","kdf","y16","x16","qnw"]
-- *Day24> swaps = [(a,b) | a <- ["z15","dnn","rjm","ctg","dqg","gnc","mrm"], b <- ["z16","kdf","qnw"], canSwap ops' a b]
-- *Day24> helps ops' (replicate 14 0 ++ [1], replicate 14 0 ++ [1]) swaps
-- [("z15","z16"),("z15","qnw"),("dnn","z16"),("dnn","qnw"),("mrm","z16"),("mrm","qnw")]
-- *Day24> helps ops' (replicate 15 0 ++ [1], replicate 15 0 ++ [1]) swaps
-- [("z15","z16"),("z15","kdf"),("z15","qnw"),("mrm","z16"),("mrm","kdf"),("mrm","qnw")]
-- *Day24> helps ops' (replicate 14 0 ++ [1,1], replicate 14 0 ++ [1]) swaps
-- [("z15","z16"),("z15","kdf"),("z15","qnw"),("dnn","z16"),("dnn","kdf"),("dnn","qnw")]

-- the intersection of the above lists: [("z15","z16"),("z15","qnw")]

-- mapM_ print $ traces (swap "z15" "qnw" ops')
--  => looks better than the alternative

-- let ops'' = swap "z15" "qnw" ops'

-- *Day24> getsWrong ops''
-- [28,37]
