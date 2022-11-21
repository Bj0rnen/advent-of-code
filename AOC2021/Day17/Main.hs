{-# LANGUAGE OverloadedStrings #-}

module AOC2021.Day17.Main where

import AOC2021.Common (Parser, justParse)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (eol)
import Data.Array (Ix(range, inRange))
import Data.List (scanl')

targetArea :: Parser ((Int, Int), (Int, Int))
targetArea = do
    "target area: x="
    x1 <- L.signed (pure ()) L.decimal
    ".."
    x2 <- L.signed (pure ()) L.decimal
    ", y="
    y1 <- L.signed (pure ()) L.decimal
    ".."
    y2 <- L.signed (pure ()) L.decimal
    eol
    return ((x1, y1), (x2, y2))

solve1 :: ((Int, Int), (Int, Int)) -> Int
solve1 ((_, y1), (_, _))
  | y1 < 0 = sum [1 .. -y1 - 1]
  | otherwise = error "Case where target isn't below (0, 0) not implemented"

hitsTarget :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
hitsTarget ((x1, y1), (x2, y2)) (xv, yv) =
    inRange ((x1, y1), (x2, y2)) $ last $ takeWhile (\(x, y) -> x <= x2 && y >= y1) steps
  where
    velocities =
        iterate
            (\(xv', yv') ->
                ( if xv' > 0 then
                      xv' - 1
                  else if xv' < 0 then
                      xv' + 1
                  else 0
                , yv' - 1))
            (xv, yv)
    steps = scanl' (\(x, y) (xv', yv') -> (x + xv', y + yv')) (0, 0) velocities

solve2 :: ((Int, Int), (Int, Int)) -> Int
solve2 ((x1, y1), (x2, y2))
  | y1 >= 0 = error "Case where target isn't below (0, 0) not implemented"
  | x1 < 0  = error "Case where target is left of (0, 0) not implemented"
  | otherwise = length $ filter (hitsTarget ((x1, y1), (x2, y2))) [(xv, yv) | xv <- xCandidates, yv <- yCandidates]
  where
    yCandidates = [y1 .. -y1 - 1]
    xCandidates = [0 .. x2]

main :: IO ()
main = do
    input <- justParse targetArea <$> readFile "AOC2021/Day17/input.txt"
    print $ solve1 input
    print $ solve2 input
