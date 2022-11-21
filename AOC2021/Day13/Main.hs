{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module AOC2021.Day13.Main where

import Text.Megaparsec.Char.Lexer (decimal)
import AOC2021.Common (Parser, justParse, Matrix, mapUArray, showCharMatrix)
import Control.Applicative (Alternative(some, (<|>)))
import Control.Applicative.Combinators (manyTill)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec (MonadParsec(eof))
import Data.Array.Unboxed (accumArray, elems, IArray (bounds), (!), array, Ix (range))
import Control.Arrow (Arrow(first))
import Data.List (foldl')

point :: Parser (Int, Int)
point = do
    x <- decimal
    ","
    y <- decimal
    return (x, y)

data Fold = X Int | Y Int
    deriving (Show)

foldInstruction :: Parser Fold
foldInstruction = do
    "fold along "
    X <$> ("x=" *> decimal) <|> Y <$> ("y=" *> decimal)

manual :: Parser ([(Int, Int)], [Fold])
manual = do
    points <- manyTill (point <* eol) eol
    foldInstructions <- manyTill (foldInstruction <* eol) eof
    return (points, foldInstructions)

pointsToPaper :: [(Int, Int)] -> Matrix Bool
pointsToPaper points =
    accumArray (\_ () -> True) False ((0, 0), (maxX, maxY)) [(point, ()) | point <- points]
  where
    maxX = maximum $ map fst points
    maxY = maximum $ map snd points

fold :: Matrix Bool -> Fold -> Matrix Bool
fold paper (X x) =
    array newBounds [((px, py), paper ! (px, py) || paper ! (maxX - px, py)) | (px, py) <- range newBounds]
  where
    (_, (maxX, maxY)) = bounds paper
    newBounds = ((0, 0), (x - 1, maxY))
fold paper (Y y) =
    array newBounds [((px, py), paper ! (px, py) || paper ! (px, maxY - py)) | (px, py) <- range newBounds]
  where
    (_, (maxX, maxY)) = bounds paper
    newBounds = ((0, 0), (maxX, y - 1))

solve1 :: Matrix Bool -> [Fold] -> Int
solve1 paper (fld : _) = length $ filter (== True) $ elems $ fold paper fld

solve2 :: Matrix Bool -> [Fold] -> IO ()
solve2 paper folds =
    putStrLn $ showCharMatrix $ mapUArray (\case False -> '.'; True -> '#') $ foldl' fold paper folds

main :: IO ()
main = do
    (paper, folds) <- first pointsToPaper . justParse manual <$> readFile "AOC2021/Day13/input.txt"
    print $ solve1 paper folds
    solve2 paper folds
