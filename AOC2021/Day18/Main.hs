module AOC2021.Day18.Main where

import AOC2021.Common (justParse, Parser)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative (Alternative((<|>)))
import Text.Megaparsec.Char (char)
import Data.List (foldl1')

data SnailfishNumber = Regular !Int | Pair !SnailfishNumber !SnailfishNumber
    deriving (Show)

snailfishNumber :: Parser SnailfishNumber
snailfishNumber = regular <|> pair
  where
    regular = Regular <$> L.decimal
    pair = do
        char '['
        x <- snailfishNumber
        char ','
        y <- snailfishNumber
        char ']'
        return $ Pair x y

leftmostExplosion :: SnailfishNumber -> Maybe (Int, (Int, Int))
leftmostExplosion n =
    case go n 0 0 of
        Left explosion -> Just explosion
        Right _ -> Nothing
  where
    go :: SnailfishNumber -> Int -> Int -> Either (Int, (Int, Int)) Int
    go (Regular _) _ i = Right (i + 1)
    go (Pair (Regular x) (Regular y)) 4 i = Left (i, (x, y))
    go (Pair x y) d i = do
        i' <- go x (d + 1) i
        go y (d + 1) i'

leftmostSplit :: SnailfishNumber -> Maybe Int
leftmostSplit n =
    case go n 0 of
        Left i -> Just i
        Right _ -> Nothing
  where
    go :: SnailfishNumber -> Int -> Either Int Int
    go (Regular x) i
        | x < 10 = Right (i + 1)
        | otherwise = Left i
    go (Pair x y) i = do
        i' <- go x i
        go y i'

explode :: Int -> (Int, Int) -> SnailfishNumber -> SnailfishNumber
explode i (explodeX, explodeY) n = snd (go n 0 0)
  where
    go :: SnailfishNumber -> Int -> Int -> (Maybe Int, SnailfishNumber)
    go (Regular x) _ j
      | j == i - 1 = (Just (j + 1), Regular (x + explodeX))
      | j == i + 1 = (Nothing, Regular (x + explodeY))
      | otherwise = (Just (j + 1), Regular x)
    go (Pair x y) d j
      | d == 4 && j == i = (Just (j + 1), Regular 0)
      | otherwise =
        case go x (d + 1) j of
            (Nothing, x') -> (Nothing, Pair x' y)
            (Just j', x') ->
                case go y (d + 1) j' of
                    (Nothing, y') -> (Nothing, Pair x' y')
                    (Just j'', y') -> (Just j'', Pair x' y')

split :: Int -> SnailfishNumber -> SnailfishNumber
split i n = snd (go n 0)
  where
    go :: SnailfishNumber -> Int -> (Maybe Int, SnailfishNumber)
    go (Regular x) j
      | j == i = (Nothing, Pair (Regular (x `div` 2)) (Regular ((x + 1) `div` 2)))
      | otherwise = (Just (j + 1), Regular x)
    go (Pair x y) j =
        case go x j of
            (Nothing, x') -> (Nothing, Pair x' y)
            (Just j', x') ->
                case go y j' of
                    (Nothing, y') -> (Nothing, Pair x' y')
                    (Just j'', y') -> (Just j'', Pair x' y')

reduce :: SnailfishNumber -> SnailfishNumber
reduce n =
    case (leftmostExplosion n, leftmostSplit n) of
        (Nothing, Nothing) -> n
        (Just (i, (explodeX, explodeY)), _) -> reduce (explode i (explodeX, explodeY) n)
        (_, Just i) -> reduce (split i n)

plus :: SnailfishNumber -> SnailfishNumber -> SnailfishNumber
plus n m = reduce (Pair n m)

magnitude :: SnailfishNumber -> Int
magnitude (Regular i) = i
magnitude (Pair x y) = 3 * magnitude x + 2 * magnitude y

solve1 :: [SnailfishNumber] -> Int
solve1 xs = magnitude $ foldl1' plus xs

solve2 :: [SnailfishNumber] -> Int
solve2 xs =
    maximum $ [magnitude (x `plus` y) | x <- xs, y <- xs]

main :: IO ()
main = do
    numbers <- map (justParse snailfishNumber) . lines <$> readFile "AOC2021/Day18/input.txt"
    print $ solve1 numbers
    print $ solve2 numbers
