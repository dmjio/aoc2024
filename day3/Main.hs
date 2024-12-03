import Control.Monad
import Data.List
import Data.Char
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  part1
  part2

example1 :: String
example1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

example2 :: String
example2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

parse1 :: IO Int
parse1 = do
  -- string <- pure example1
  consume <$> readFile "input.txt"

parse2 :: IO Int
parse2 = do
  -- string <- pure example2
  consumeDo <$> readFile "input.txt"

consume :: [Char] -> Int
consume xs = go xs 0
  where
    go [] acc = acc
    go xs acc
      | [(n,rest)] <- readP_to_S mul xs = go rest (n + acc)
      | otherwise = go (drop 1 xs) acc

consumeDo :: [Char] -> Int
consumeDo xs = go xs 0 True
  where
    go [] acc _ = acc
    go xs acc enabled
      | (_,ys) : _ <- readP_to_S do_ xs
      = go ys acc True
      | (_,ys) : _ <- readP_to_S dont xs
      = go ys acc False
      | (n,ys) : _ <- readP_to_S mul xs, enabled
      = go ys (n + acc) enabled
      | (_,ys) :_  <- readP_to_S mul xs, not enabled
      = go ys acc enabled
      | otherwise
      = go (drop 1 xs) acc enabled

mul :: ReadP Int
mul = do
  string "mul"
  char '('
  l <- read <$> munch isDigit
  char ','
  r <- read <$> munch isDigit
  char ')'
  pure (l * r)

do_ :: ReadP ()
do_ = () <$ string "do()"

dont :: ReadP ()
dont = () <$ string "don't()"

part1 :: IO ()
part1 = print =<< parse1

part2 :: IO ()
part2 = print =<< parse2
