module Main where

import           Control.Monad      ((<=<))
import           Data.Array         (Array, listArray, assocs, bounds, (!))
import qualified Data.Array         as A
import           Data.Foldable      (toList)
import           Data.Maybe
import           Data.Set           (Set)
import qualified Data.Set           as S
import           Debug.Trace        (traceShow)
import           System.Environment

main :: IO ()
main = do
  let defaultMax = 10000
  itermax <- fromMaybe defaultMax . listToMaybe . fmap read <$> getArgs
  -- s <- build <$> pure grid
  s <- build <$> readFile "input.txt"
  print (part1 s)
  print (part2 s itermax)

part1 :: Grid -> Int
part1 = positions

part2 :: Grid -> Int -> Int
part2 = loops

type Grid = Array Int (Array Int Char)

build :: String -> Grid
build = go . lines
  where
    go :: [String] -> Grid
    go xss = listArray (0, length xss - 1)
      [ listArray (0, length xs - 1) xs
      | xs <- xss
      ]

pprint :: Grid -> String
pprint g = unlines [ toList arr | arr <- toList g ]

grid =
 "....#.....\n\
 \.........#\n\
 \..........\n\
 \..#.......\n\
 \.......#..\n\
 \..........\n\
 \.#..^.....\n\
 \........#.\n\
 \#.........\n\
 \......#..."

start :: Grid -> (Int,Int)
start grid = head
  [ (y,x)
  | (y, row) <- assocs grid
  , (x, char) <- assocs row
  , char == '^'
  ]

data Dir = N | S | E | W
  deriving (Show, Eq)

right :: Dir -> Dir
right N = E
right E = S
right S = W
right W = N

move :: Dir -> (Int, Int) -> (Int, Int)
move N (col,row) = (col - 1, row)
move S (col,row) = (col + 1, row)
move E (col,row) = (col, row + 1)
move W (col,row) = (col, row - 1)

step :: Grid -> (Int,Int) -> Dir -> (Maybe Char, (Int,Int))
step grid current dir =
  case get grid next of
    Nothing -> (Nothing, next)
    Just char -> (Just char, next)
  where
    next = move dir current

safeIndex :: Int -> Array Int a -> Maybe a
safeIndex x m
  | x >= low && x <= high = Just (m ! x)
  | otherwise = Nothing
    where
      (low, high) = bounds m

get :: Grid -> (Int, Int) -> Maybe Char
get m (y,x) = (safeIndex x <=< safeIndex y) m

positions :: Grid -> Int
positions grid = S.size (trace (start grid) N mempty)
  where
    trace coord dir seen = do
      case step grid coord dir of
        (Nothing, _)     -> S.insert coord seen
        (Just '#', _)    -> trace coord (right dir) seen
        (Just '.', next) -> trace next dir (S.insert coord seen)
        (Just '^', next) -> trace next dir (S.insert coord seen)

obstructions :: Grid -> [(Int,Int)]
obstructions grid =
  [ (y, x)
  | (y, row) <- assocs grid
  , (x, char) <- assocs row
  , char == '.'
  ]

loops :: Grid -> Int -> Int
loops grid itermax = length
  [ o
  | o <- obstructions grid
  , loop (obstruct grid o) itermax
  ]

obstruct :: Grid -> (Int, Int) -> Grid
obstruct grid (y,x) = do
  grid A.//
    [ (y, row A.// [(x, '#')])
    | Just row <- pure (safeIndex y grid)
    ]

loop :: Grid -> Int -> Bool
loop grid itermax = trace (start grid) N 0
  where
    trace coord dir i | i > itermax = True
    trace coord dir !i = do
      case step grid coord dir of
        (Nothing, _)     -> False
        (Just '#', _)    -> trace coord (right dir) (i + 1)
        (Just '.', next) -> trace next dir (i + 1)
        (Just '^', next) -> trace next dir (i + 1)



