module AOC2021.Day25.Main where

import AOC2021.Common (Matrix, listMatrix2dUInferDimensions, iterateUntilUnchangedWithIndex, printMatrix, printCharMatrix)
import Data.Array.Unboxed (IArray(bounds), (!), array, Ix (range))

stepEast :: Matrix Char -> Matrix Char
stepEast m = array bnds [((x, y), f (x, y)) | (x, y) <- range bnds]
  where
    bnds@((_, _), (maxX, _)) = bounds m
    width = maxX + 1
    westWraparound (x, y) = ((x - 1) `mod` width, y)
    eastWraparound (x, y) = ((x + 1) `mod` width, y)
    f (x, y) = case m ! (x, y) of
        '.' -> if west == '>' then '>' else '.'
        '>' -> if east == '.' then '.' else '>'
        'v' -> 'v'
      where
        west = m ! westWraparound (x, y)
        east = m ! eastWraparound (x, y)

stepSouth :: Matrix Char -> Matrix Char
stepSouth m = array bnds [((x, y), f (x, y)) | (x, y) <- range bnds]
  where
    bnds@((_, _), (_, maxY)) = bounds m
    height = maxY + 1
    northWraparound (x, y) = (x, (y - 1) `mod` height)
    southWraparound (x, y) = (x, (y + 1) `mod` height)
    f (x, y) = case m ! (x, y) of
        '.' -> if north == 'v' then 'v' else '.'
        'v' -> if south == '.' then '.' else 'v'
        '>' -> '>'
      where
        north = m ! northWraparound (x, y)
        south = m ! southWraparound (x, y)

step :: Matrix Char -> Matrix Char
step = stepSouth . stepEast

solve1 :: Matrix Char -> Int
solve1 m = snd (iterateUntilUnchangedWithIndex step m) + 1

solve2 :: Matrix Char -> Int
solve2 m = snd (iterateUntilUnchangedWithIndex step m) + 1

main :: IO ()
main = do
    m <- listMatrix2dUInferDimensions . lines <$> readFile "AOC2021/Day25/input.txt"
    print $ solve1 m
    print $ solve2 m