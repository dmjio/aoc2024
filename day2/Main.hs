import Control.Monad
import Data.List

main :: IO ()
main = do
  part1
  part2

example =
  "7 6 4 2 1\n\
  \1 2 7 8 9\n\
  \9 7 6 2 1\n\
  \1 3 2 4 5\n\
  \8 6 4 4 1\n\
  \1 3 6 7 9\n"

parse :: IO [[Int]]
parse = do
  -- string <- pure example
  string <- readFile "input.txt"
  pure $ fmap read <$> fmap words (lines string)

part1 :: IO ()
part1 = do
  xxs <- parse
  print $ length (filter isSafe xxs)

part2 :: IO ()
part2 = do
  xxs <- parse
  print $ length (filter withDapenener xxs)
    where
      withDapenener :: [Int] -> Bool
      withDapenener xs = isSafe xs || tolerate xs

      tolerate :: [Int] -> Bool
      tolerate xs = any isSafe (combos xs)

      combos :: [a] -> [[a]]
      combos xs =
        [ case splitAt n xs of
            (ls, _ : rs) ->
              ls ++ rs
        | n <- [ 0 .. length xs - 1]
        ]

isSafe :: [Int] -> Bool
isSafe xs = and
  [ increasing xs || decreasing xs
  , legit xs
  ]

f :: (b -> b -> Bool) -> [b] -> Bool
f g xs = all (uncurry g) $ zip xs (tail xs)

increasing, decreasing :: [Int] -> Bool
increasing = f (<)
decreasing = f (>)

legit
  :: (Eq a, Num a, Enum a)
  => [a]
  -> Bool
legit xs
  = all (`elem` [1..3])
  $ zipWith ((abs .) . subtract) xs (tail xs)
