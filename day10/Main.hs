import           Control.Monad
import           Data.Array      (Array)
import           Data.Array.Base
import           Data.Char
import           Data.Foldable
import           Data.Maybe
import           Data.Set        (Set)
import qualified Data.Set        as S

type Grid  = Array Int (Array Int Char)
type Coord = (Int, Int)

puzzle :: String
puzzle =
  "89010123\n\
  \78121874\n\
  \87430965\n\
  \96549874\n\
  \45678903\n\
  \32019012\n\
  \01329801\n\
  \10456732"

main :: IO ()
main = do
  s <- pure puzzle
  s <- readFile "input.txt"
  print (part1 s)
  print (part2 s)

part1 :: String -> Int
part1 s = sum $ trailheads grid allDistinct (heads grid)
  where
    allDistinct = False
    grid = build s

part2 :: String -> Int
part2 s = sum $ trailheads grid allDistinct (heads grid)
  where
    allDistinct = True
    grid = build s

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

get :: Grid -> Coord -> Maybe Char
get m (y,x) = (safeIndex x <=< safeIndex y) m
  where
    safeIndex :: Int -> Array Int a -> Maybe a
    safeIndex x m = m !? x

peaks :: Grid -> Set Coord
peaks = search '9'

heads :: Grid -> [Coord]
heads = S.toList . search '0'

search :: Char -> Grid -> Set Coord
search x grid = foldl' f mempty coords
  where
    f :: Set Coord -> Coord -> Set Coord
    f acc c
      | Just y <- get grid c
      , x == y    = S.insert c acc
      | otherwise = acc

    coords = liftA2 (,) [0..n] [0..n]
    (_, n) = bounds grid

trailhead :: Bool -> Grid -> Coord -> Int
trailhead allDistinct grid start =
    dfs (peaks grid) mempty [start] 0
  where
    dfs _ _ [] acc = acc

    dfs peaks seen (c:cs) acc
      | c `S.member` peaks
      = dfs peaks (S.insert c seen) cs (acc + 1)
      | otherwise
      = dfs peaks (S.insert c seen) (ns <> cs) acc
      where
        ns = [ n
             | n <- neighbors grid c
             , canMove grid c n
             , allDistinct || S.notMember n seen
             ]

canMove
  :: Grid
  -> Coord
  -> Coord
  -> Bool
canMove grid current next =
  case (get grid current, get grid next) of
    (Just c, Just n) ->
      digitToInt c + 1 == digitToInt n
    _ -> False

trailheads
  :: Grid
  -> Bool
  -> [Coord]
  -> [Int]
trailheads grid distinct coords =
  trailhead distinct grid <$> coords

neighbors :: Grid -> Coord -> [Coord]
neighbors grid (y,x) =
  [ (y + 1, x)
  , (y - 1, x)
  , (y, x + 1)
  , (y, x - 1)
  ]



