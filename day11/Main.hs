module Main where

import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  putStrLn "part1"
  print $ V.length (blinks 25 puzzle)

puzzle :: Vector Int
puzzle = V.fromList (read <$> words string)
  where
    string = "2 77706 5847 9258441 0 741 883933 12"

test :: [Int]
test = [125, 17]

change :: Int -> Vector Int
change 0 = V.singleton 1
change n
  | let digits = toDigits n
  , even (V.length digits)
  , (ls, rs) <- V.splitAt (V.length digits `div` 2) digits
  = V.singleton (fromDigits ls) <> V.singleton (fromDigits rs)
  | otherwise = V.singleton (n * 2024)

blink :: Vector Int -> Vector Int
blink = V.concatMap change

blinks :: Int -> Vector Int -> Vector Int
blinks n xs = iterate blink xs !! n

toDigits :: Int -> Vector Int
toDigits = V.reverse . V.unfoldr go
  where
    go :: Int -> Maybe (Int, Int)
    go n
      | n < 1 = Nothing
      | otherwise =
          case quotRem n 10 of
            (ls, rs) -> Just (rs, ls)

fromDigits :: Vector Int -> Int
fromDigits = V.foldl' go 0
  where
    go :: Int -> Int -> Int
    go acc x = x + (10 * acc)


