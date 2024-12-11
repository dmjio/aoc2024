import           Control.Monad
import           Data.Array
import           Data.Array.Base ((!?))
import           Data.Foldable
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Ratio
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Data.Tuple

type Frequency = Char

type Grid = Array Int (Array Int Frequency)

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

main :: IO ()
main = do
  -- s <- pure puzzle
  s <- readFile "input.txt"
  print (part1 s)
  print (part2 s)

nodes :: GetNodes -> String -> Int
nodes f s = length $ antinodes f grid (antennas grid)
  where
    grid = build s

part1 :: String -> Int
part1 = nodes part1Antinodes

part2 :: String -> Int
part2 = nodes part2Antinodes

type Coord = (Int, Int)

get :: Grid -> (Int, Int) -> Maybe Frequency
get m (y,x) = (safeIndex x <=< safeIndex y) m
  where
    safeIndex :: Int -> Array Int a -> Maybe a
    safeIndex x m = m !? x

type Antennas = M.Map Frequency (S.Set Coord)

antennas :: Grid -> Antennas
antennas grid = foldl' process mempty (coords grid)
  where
    process
      :: Antennas
      -> Coord
      -> Antennas
    process m coord
      | Just '.' <- get grid coord = m
      | Just c <- get grid coord
      = addCoord c coord m
      | otherwise = m

    addCoord key coord =
      M.unionWith S.union
        (M.singleton key (S.singleton coord))

combos :: Set Coord -> [(Coord, Coord)]
combos cs = nubBy (\x y -> x == swap y)
  [ (x,y)
  | x <- S.toList cs
  , y <- tail (S.toList cs)
  , x /= y
  , swap x /= y
  ]

type GetNodes = Grid -> Coord -> Coord -> Set Coord

antinodes
  :: GetNodes -> Grid -> Antennas -> Set Coord
antinodes getNodes grid antennas = foldMap process (M.elems antennas)
  where
    process :: Set Coord -> Set Coord
    process cs = foldr findAntinodes mempty (combos cs)

    findAntinodes
      :: (Coord, Coord)
      -> Set Coord
      -> Set Coord
    findAntinodes = S.union . uncurry (getNodes grid)

part2Antinodes :: Grid -> Coord -> Coord -> Set Coord
part2Antinodes grid (y1,x1) (y2, x2) = S.unions
  [ S.fromList
    [ c
    | c <- [ (y1 + (dy1 * n), x1 + (dx1 * n))
           , (y2 + (dy2 * n), x2 + (dx2 * n))
           ]
    , bounded c grid
    ]
  | n <- [ 0 .. bound ]
  ]
  where
    (dy1, dx1) = (y1 - y2, x1 - x2)
    (dy2, dx2) = (y2 - y1, x2 - x1)
    (_, bound) = bounds grid

part1Antinodes :: Grid -> Coord -> Coord -> Set Coord
part1Antinodes grid (y1,x1) (y2, x2) = S.fromList
  [ c
  | c <- [ (y1 + dy1, x1 + dx1)
         , (y2 + dy2, x2 + dx2)
         ]
  , bounded c grid
  ]
  where
    (dy1, dx1) = (y1 - y2, x1 - x2)
    (dy2, dx2) = (y2 - y1, x2 - x1)

bounded :: Coord -> Grid -> Bool
bounded (x,y) grid =
  and
  [ x >= 0 && x <= bound
  , y >= 0 && y <= bound
  ] where
     (_, bound) = bounds grid

coords :: Grid -> [Coord]
coords array = (,) <$> [ 0 .. n ] <*> [ 0 .. n ]
  where
    (_, n) = bounds array

puzzle =
  "............\n\
  \........0...\n\
  \.....0......\n\
  \.......0....\n\
  \....0.......\n\
  \......A.....\n\
  \............\n\
  \............\n\
  \........A...\n\
  \.........A..\n\
  \............\n\
  \............"
