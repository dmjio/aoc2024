module Main where

import Control.Monad (replicateM)
import Data.List.Split (splitOn)

input =
  "190: 10 19\n\
  \3267: 81 40 27\n\
  \83: 17 5\n\
  \156: 15 6\n\
  \7290: 6 8 6 15\n\
  \161011: 16 10 13\n\
  \192: 17 8 14\n\
  \21037: 9 7 18 13\n\
  \292: 11 6 16 20\n"

main :: IO ()
main = do
  -- s <- pure input
  s <- readFile "input.txt"
  let parsed = fmap parse (lines s)
  print (part1 parsed)
  print (part2 parsed)

part1 = run [ Add, Mul ]
part2 = run [ Add, Mul, Con ]

run :: [Op] -> [(Int, [Int])] -> Int
run ops inputs =
 sum [ test
     | (test, values) <- inputs
     , any (==test) (eval ops values)
     ]

parse :: String -> (Int, [Int])
parse s =
  case (takeWhile (/=':') s, dropWhile (/=':') s) of
    (k, _ : _ : xs) ->
      (read k, fmap read (splitOn " " xs))

data Op = Add | Mul | Con
  deriving (Show)

interp :: Int -> [(Op, Int)] -> Int
interp = foldr $ \(f, y) x -> apply f x y

eval :: [Op] -> [Int] -> [Int]
eval ops (x:xs) =
  [ interp x (zipWith (,) es xs)
  | es <- replicateM (length xs) ops
  ]

apply Add x y = x + y
apply Mul x y = x * y
apply Con x y = read (show x <> show y)
