import           Data.Char
import           Data.Function
import           Data.List
import           Data.List.Split
import qualified Data.Map                     as M
import           Text.ParserCombinators.ReadP

main = do
  -- s <- lines <$> readFile "sample.txt"
  s <- lines <$> readFile "input.txt"
  let [rules, rawPaths] = splitOn [""] s
      paths = fmap read <$> splitOn "," <$> rawPaths
  print (part1 rules paths)
  print (part2 rules paths)

part1, part2 :: [String] -> [[Int]] -> Int
part1 rules paths = sum $
  [ middle path
  | (path, corrected) <- zip paths (patch rules <$> paths)
  , path == corrected
  ]
part2 rules paths = sum
  [ middle corrected
  | (path, corrected) <- zip paths (patch rules <$> paths)
  , path /= corrected
  ]

patch :: [String] -> [Int] -> [Int]
patch rules path = patched
  where
    patched = fst <$> sortBy condition z
    condition = mconcat
      [ compare `on` (fst . snd)
      , flip compare `on` (snd . snd)
      ]
    t = adjacency $ \x y -> (y, [x])
    t' = adjacency $ \x y -> (x, [y])
    z = [ (p, (l,r))
        | p <- path
        , let l = length (M.findWithDefault [] p t)
              r = length (M.findWithDefault [] p t')
        ]
    adjacency f = M.fromListWith (++)
      [ f x y
      | (x,y) <- parse <$> rules
      , null $ [x,y] \\ path
      ]

middle :: [Int] -> Int
middle xs =
  case splitAt (length xs `div` 2) xs of
    (_, x : _) -> x

parse :: String -> (Int, Int)
parse s | (x, _) : _ <- flip readP_to_S s $ do
  l <- read <$> munch1 isDigit
  char '|'
  r <- read <$> munch1 isDigit
  pure (l,r) = x
