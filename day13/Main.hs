{-# LANGUAGE RecordWildCards #-}
import Control.Applicative
import Control.Monad.State
import Data.Char
import Data.List.Split
import Data.Bool
import Data.Function
import Data.List
import Data.Maybe

puzzle :: String
puzzle =
  "Button A: X+94, Y+34\n\
  \Button B: X+22, Y+67\n\
  \Prize: X=8400, Y=5400\n\
  \\n\
  \Button A: X+26, Y+66\n\
  \Button B: X+67, Y+21\n\
  \Prize: X=12748, Y=12176\n\
  \\n\
  \Button A: X+17, Y+86\n\
  \Button B: X+84, Y+37\n\
  \Prize: X=7870, Y=6450\n\
  \\n\
  \Button A: X+69, Y+23\n\
  \Button B: X+27, Y+71\n\
  \Prize: X=18641, Y=10279"

main :: IO ()
main = do
  s <- fmap unwords . splitOn [""] . lines <$> pure puzzle
  s <- fmap unwords . splitOn [""] . lines <$> readFile "input.txt"
  print $ part1 (parse <$> s)

data Input = Input { ax,ay,bx,by,px,py :: Int }
  deriving (Show)

part1 :: [Input] -> Int
part1 xs = sum [ x | Just x <- cheapest . solve 100 <$> xs ]

cheapest :: [(Int,Int)] -> Maybe Int
cheapest xs = price <$> listToMaybe (sortBy (compare `on` price) xs)
  where
    price (x,y) = x * 3 + y

solve :: Int -> Input -> [(Int,Int)]
solve n Input {..} =
  [ (a,b)
  | a <- [0..n]
  , b <- [0..n]
  , ax*a + bx*b == px
  , ay*a + by*b == py
  ]

parse :: String -> Input
parse s =
  runParse s $ do
    (ax,ay) <- chunk "Button A" <* char ' '
    (bx,by) <- chunk "Button B" <* char ' '
    (px,py) <- chunk "Prize"
    pure Input {..}
  where
    chunk :: String -> Parser (Int,Int)
    chunk name = do
      string (name <> ": ")
      char 'X'
      char '=' <|> char '+'
      l <- read <$> some (satisfy isDigit)
      string ", Y"
      char '=' <|> char '+'
      r <- read <$> some (satisfy isDigit)
      pure (l, r)

type Parser a = StateT String [] a

runParse :: String -> Parser a -> a
runParse s p =
  case runStateT p s of
    (x, []) : _ -> x
    _ -> error "failed to parse"

sepBy1 :: Alternative m => m sep -> m a -> m [a]
sepBy1 sep p = (:) <$> p <*> many (sep *> p)

string :: String -> Parser String
string = traverse char

char :: Char -> Parser Char
char c = satisfy (==c)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f =
  StateT $ \s ->
    case s of
      [] -> []
      (c:cs) | f c -> [(c,cs)]
             | otherwise -> []
