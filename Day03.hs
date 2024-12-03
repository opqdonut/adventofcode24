module Day03 where

import Data.List
import Data.Char

slurp = readFile "input-03"

type Parser a = String -> Maybe (a,String)

infixr 9 `followedBy`

followedBy :: Parser a -> Parser b -> Parser (a,b)
followedBy pa pb xs = do
  (a,xs') <- pa xs
  (b,xs'') <- pb xs'
  return ((a,b),xs'')

orThen :: Parser a -> Parser a -> Parser a
orThen pa pb xs = case pa xs of
  Just ok -> Just ok
  Nothing -> pb xs

expect :: String -> Parser String
expect s xs = case stripPrefix s xs of
                Just xs' -> Just (s, xs')
                Nothing -> Nothing

number :: Parser Int
number xs = case span isDigit xs of
              ([],_) -> Nothing
              (ds,xs') -> Just (read ds, xs')

pmap :: (a->b) -> Parser a -> Parser b
pmap f pa xs = fmap (\(a,xs) -> (f a,xs)) (pa xs)

data Op = Mul Int Int | Do | Dont
  deriving (Show,Eq)

parse_mul =
  pmap (\(_,(a,(_,(b,_)))) -> Mul a b) $
  (expect "mul(")
  `followedBy` number
  `followedBy` (expect ",")
  `followedBy` number
  `followedBy` (expect ")")

parse_do = pmap (const Do) $ expect "do()"

parse_dont = pmap (const Dont) $ expect "don't()"

parse_op = parse_mul `orThen` parse_do `orThen` parse_dont

uncorrupted [] = []
uncorrupted xs = case parse_op xs
  of Just (m,xs') -> m:uncorrupted xs'
     Nothing -> uncorrupted (tail xs)

example = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
example2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

part1 s = sum $ map ev $ uncorrupted s
  where ev (Mul a b) = a*b
        ev _ = 0

part2 :: String -> Int
part2 s = ev 0 $ uncorrupted s
  where ev acc [] = acc
        ev acc ((Mul a b):xs) = ev (acc + a*b) xs
        ev acc (Do:xs) = ev acc xs
        ev acc (Dont:xs) = noev acc xs
        noev acc [] = acc
        noev acc (Do:xs) = ev acc xs
        noev acc (_:xs) = noev acc xs

--part2 example2

main =
  --print . part1 =<< slurp
  print . part2 =<< slurp
