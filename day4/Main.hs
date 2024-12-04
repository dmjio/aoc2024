import           Control.Monad
import           Data.Array
import qualified Data.Array    as A
import           Data.List
import           Data.Maybe
import qualified Data.Set      as S

type Matrix = Array Int (Array Int Char)

type Coord = (Int,Int)
type Path = [Coord]

ex1 = "..X...\n\
      \.SAMX.\n\
      \.A..A.\n\
      \XMAS.S\n\
      \.X...."

ex2 =
  ".M.S......\n\
  \..A..MSMS.\n\
  \.M.S.MAA..\n\
  \..A.ASMSM.\n\
  \.M.S.M....\n\
  \..........\n\
  \S.S.S.S.S.\n\
  \.A.A.A.A..\n\
  \M.M.M.M.M.\n\
  \.........."

puzzle =
  "MMMSXXMASM\n\
  \MSAMXMSMSA\n\
  \AMXSXMAAMM\n\
  \MSAMASMSMX\n\
  \XMASAMXAMM\n\
  \XXAMMXXAMA\n\
  \SMSMSASXSS\n\
  \SAXAMASAAA\n\
  \MAMMMXMMMM\n\
  \MXMXAXMASX"

main :: IO ()
main = do
  xs <- lines <$> readFile "input.txt"
  print $ part1 (build xs)
  print $ part2 (build xs)

build :: [String] -> Matrix
build xss = listArray (0, length xss - 1)
  [ listArray (0, length xs - 1) xs
  | xs <- xss
  ]

safeIndex :: Int -> Array Int a -> Maybe a
safeIndex x m
  | x >= low && x <= high = Just (m A.! x)
  | otherwise = Nothing
    where
      (low, high) = bounds m

get :: Matrix -> (Int, Int) -> Maybe Char
get m (y,x) = (safeIndex x <=< safeIndex y) m

getPath :: [(Int,Int)] -> Matrix -> String
getPath xs m = catMaybes (get m <$> xs)

neighbors c@(x,y) d = fmap (c:) $
  [ [ (x-n, y)   | n <- [ 1 .. d ] ]
  , [ (x-n, y-n) | n <- [ 1 .. d ] ]
  , [ (x-n, y+n) | n <- [ 1 .. d ] ]
  , [ (x+n, y)   | n <- [ 1 .. d ] ]
  , [ (x+n, y-n) | n <- [ 1 .. d ] ]
  , [ (x+n, y+n) | n <- [ 1 .. d ] ]
  , [ (x, y+n)   | n <- [ 1 .. d ] ]
  , [ (x, y-n)   | n <- [ 1 .. d ] ]
  ]

part1 :: Matrix -> Int
part1 m = length (foldMap go coords)
  where
    coords = liftA2 (,) [0..n] [0..n]
    (_, n) = bounds m
    go coord = S.fromList
      [ path
      | path <- neighbors coord (length "XMAS" - 1)
      , getPath path m == "XMAS"
      ]

part2 :: Matrix -> Int
part2 m = length (foldl' go mempty coords)
  where
    coords = liftA2 (,) [0..n] [0..n]
    (_, n) = bounds m
    go seen coord = result
      where
        result
          | (ls, rs) <- neighborsXmas coord
          , getPath ls m `elem` ["SAM", "MAS"]
          , getPath rs m `elem` ["SAM", "MAS"]
          , let x = S.fromList (ls ++ rs)
          , not (S.member x seen)
          = S.insert x seen
          | otherwise
          = seen

neighborsXmas c@(x,y) = (l,r)
  where
    l = [ (x+1, y-1), c, (x-1, y+1) ]
    r = [ (x+1, y+1), c, (x-1, y-1) ]
