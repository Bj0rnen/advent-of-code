{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module AOC2021.Day5.Main where

import qualified Text.Megaparsec.Char.Lexer as L
import AOC2021.Common (Parser, justParse)
import Data.Array.Unboxed (elems, Ix (range), accumArray, UArray)

lineSegment :: Parser ((Int, Int), (Int, Int))
lineSegment = do
    x1 <- L.decimal
    ","
    y1 <- L.decimal
    " -> "
    x2 <- L.decimal
    ","
    y2 <- L.decimal
    return ((x1, y1), (x2, y2))

isHorizontal :: ((Int, Int), (Int, Int)) -> Bool
isHorizontal ((_, y1), (_, y2)) = y1 == y2

isVertical :: ((Int, Int), (Int, Int)) -> Bool
isVertical ((x1, _), (x2, _)) = x1 == x2

anyDirectionalRange :: Ix a => (a, a) -> [a]
anyDirectionalRange (x, y)
  | x <= y = range (x, y)
  | otherwise = reverse $ range (y, x)

lineSegmentToIndices :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
lineSegmentToIndices l@((x1, y1), (x2, y2))
  | isHorizontal l || isVertical l = anyDirectionalRange l
  | otherwise = zip (anyDirectionalRange (x1, x2)) (anyDirectionalRange (y1, y2))

mkMatrix :: [((Int, Int), (Int, Int))] -> UArray (Int, Int) Int
mkMatrix = accumArray (+) 0 ((0, 0), (999, 999)) . concatMap (map (, 1) . lineSegmentToIndices)

solve1 :: [((Int, Int), (Int, Int))] -> Int
solve1 lineSegments = length $ filter (>= 2) $ elems matrix
  where
    matrix = mkMatrix $ filter (\l -> isHorizontal l || isVertical l) lineSegments

solve2 :: [((Int, Int), (Int, Int))] -> Int
solve2 lineSegments = length $ filter (>= 2) $ elems matrix
  where
    matrix = mkMatrix lineSegments

main :: IO ()
main = do
    input <- map (justParse lineSegment) . lines <$> readFile "AOC2021/Day5/input.txt"
    print $ solve1 input
    print $ solve2 input
