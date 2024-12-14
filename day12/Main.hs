import           Data.Array.Base
import           Data.Array.Unboxed
import           Data.Char
import           Data.List
import           Data.List.Split
import qualified Data.Map           as M
import           Data.Maybe
import           Data.Set           (Set)
import qualified Data.Set           as S
import           Data.Tuple

pz =
  "AAAA\n\
  \BBCD\n\
  \BBCC\n\
  \EEEC"

puzzle =
 "RRRRIICCFF\n\
 \RRRRIICCCF\n\
 \VVRRRCCFFF\n\
 \VVRCCCJFFF\n\
 \VVVVCJJCFE\n\
 \VVIVCCJJEE\n\
 \VVIIICJJEE\n\
 \MIIIIIJJEE\n\
 \MIIISIJEEE\n\
 \MMMISSJEEE"

main = do
  s <- pure pz
  s <- readFile "input.txt"
  print (part1 s)

type Grid = UArray (Int,Int) Char
type Coord = (Int, Int)

part1 :: String -> Int
part1 s = prices
  where
    grid = build s
    prices = go (indices grid) 0
      where
        go [] acc     = acc
        go (c:cs) acc = go next (acc + price grid patch)
          where
            next  = filter (`S.notMember` patch) cs
            patch = garden grid c

build :: String -> Grid
build s = listArray bounds_ (filter isUpper s)
  where
    bounds_ = ((0,0), (n,n))
    n = length (lines s) - 1

ppr :: Grid -> String
ppr grid = unlines (chunksOf (n + 1) (elems grid))
  where
    (_, (n,_)) = bounds grid

get :: Grid -> Coord -> Maybe Char
get = (!?)

neighbors :: Grid -> Coord -> [Coord]
neighbors grid (y,x) =
  [ (y + 1, x)
  , (y - 1, x)
  , (y, x + 1)
  , (y, x - 1)
  ]

price :: Grid -> Set Coord -> Int
price grid coords = area * perimeter
  where
    area = S.size coords
    perimeter = sum
      [ S.size (ns `S.difference` coords)
      | s <- S.toList coords
      , let ns = S.fromList (neighbors grid s)
      ]

garden :: Grid -> Coord -> Set Coord
garden grid start = dfs mempty [start]
  where
    dfs seen []     = seen
    dfs seen (c:cs) = dfs (S.insert c seen) (ns <> cs)
      where
        ns = [ n
             | n <- neighbors grid c
             , isJust (get grid n)
             , get grid n == get grid c
             , S.notMember n seen
             ]
