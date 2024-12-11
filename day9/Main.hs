import           Control.Monad
import           Data.Char
import           Data.Foldable
import qualified Data.Vector.Unboxed         as V
import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector.Unboxed.Mutable as MV
import           Debug.Trace                 (traceShow)
import           GHC.ST

main :: IO ()
main = do
  s <- pure puzzle
  s <- filter isDigit <$> readFile "input.txt"
  print (part1 s)
  print (part2 s)

puzzle :: String
puzzle = "2333133121414131402"

diskmap :: String -> Vector Int
diskmap
  = V.fromList
  . concatMap go
  . zip3 [0..] duplis
  . fmap digitToInt
  where
    go (index, x, value)
      | even index = replicate value x
      | otherwise  = replicate value (-1)

    duplis = concatMap (\x -> [x,x]) [0..]

swap :: Vector Int -> Vector Int -> Vector Int -> Vector Int
swap xs ys = V.modify $ \m -> V.zipWithM_ (MV.swap m) xs ys

part1 :: String -> Int
part1 = checksum . defragBlocks . diskmap

part2 :: String -> Int
part2 = checksum . defragFiles . diskmap

defragFiles :: Vector Int -> Vector Int
defragFiles arr = go (V.maximum arr) arr
  where
    go 0 array = array
    go n array = go (n - 1) next
        where
          next | Just space <- getFreeSpaceIndices size
               = swap space fileIndices array
               | otherwise = array

          fileIndices              = getFileIndices n
          size                     = V.length fileIndices
          minIndex                 = V.head fileIndices

          getFileIndices n         = V.findIndices (== n) array
          getFreeSpaceIndices size = findSpace [ 0 .. size - 1 ]
             where
               findSpace window | any (> minIndex) window = Nothing
               findSpace window =
                 case (array V.!) <$> window of
                   xs | all (==(-1)) xs -> Just (V.fromList window)
                      | otherwise -> findSpace (fmap (+1) window)

defragBlocks :: Vector Int -> Vector Int
defragBlocks array
  | total == occuppied = array
  | otherwise = defragBlocks (swap free block array)
  where
    occuppied = V.length (V.takeWhile (>= 0) array)
    total     = V.length (V.filter (>= 0) array)
    free      = V.singleton (go 0)
      where
        go k | Just (-1) <- array V.!? k = k
             | otherwise = go (k + 1)

    block     = V.singleton (go n)
       where
         go k | Just c <- array V.!? k, c >= 0 = k
              | otherwise = go (k - 1)
         n = V.length array

checksum :: Vector Int -> Int
checksum array = sum
  [ k * v
  | (k,v) <- zip [0..] (V.toList array)
  , v /= (-1)
  ]
