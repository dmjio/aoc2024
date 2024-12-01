import Data.List
import Data.Function

main :: IO ()
main = do
  part1
  part2

example =
  "3   4\n\
  \4   3\n\
  \2   5\n\
  \1   3\n\
  \3   9\n\
  \3   3"

parse :: IO ([Int], [Int])
parse = do
  -- string <- pure example
  string <- readFile "input.txt"
  pure $ unzip
    [ (x,y)
    | [x,y] <- fmap read <$> fmap words (lines string)
    ]

part1 :: IO ()
part1 = do
  print =<< foldr go 0 . uncurry (on zip sort) <$> parse
    where
      go (x,y) acc = acc + abs (x - y)

part2 :: IO ()
part2 = do
  (lefts, rights) <- parse
  print (foldr (go rights) 0 lefts)
    where
      go rights left acc = (left * occurs) + acc
        where
          occurs = length (filter (==left) rights)

