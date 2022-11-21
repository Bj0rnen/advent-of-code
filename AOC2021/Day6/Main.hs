{-# LANGUAGE BangPatterns #-}

module AOC2021.Day6.Main where

import Data.Array.Unboxed (elems, accumArray, UArray, array, (!))
import AOC2021.Common (justParse, commaSeparatedDecimals)

day :: UArray Int Int -> UArray Int Int
day !arr = array (0, 8) [(i, f i) | i <- [0..8]]
  where
    f 8 = arr ! 0
    f 6 = arr ! 7 + arr ! 0
    f i = arr ! (i + 1)

dayN :: Int -> [Int] -> Int
dayN n input = sum $ elems finalCounts
  where
    initialCounts = accumArray (+) 0 (0,8) [(i, 1) | i <- input]
    finalCounts = iterate day initialCounts !! n

solve1 :: [Int] -> Int
solve1 = dayN 80

solve2 :: [Int] -> Int
solve2 = dayN 256

main :: IO ()
main = do
    input <- justParse commaSeparatedDecimals <$> readFile "AOC2021/Day6/input.txt"
    print $ solve1 input
    print $ solve2 input
