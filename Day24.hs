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

-- (_,ops) <- input
-- cs = candidates ops [0] (replicate nBits 1)
-- nub $ concatMap (allDeps ops) cs
