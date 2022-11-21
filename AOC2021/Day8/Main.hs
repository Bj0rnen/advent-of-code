{-# LANGUAGE OverloadedStrings #-}

module AOC2021.Day8.Main where

import AOC2021.Common (justParse, commaSeparatedDecimals, Parser)
import Text.Megaparsec (someTill)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (replicateM)
import Text.Megaparsec.Char (char, newline, letterChar, separatorChar)
import Control.Applicative (Alternative((<|>), some))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.List (find)
import Data.Set (Set)
import qualified Data.Set as Set

entry :: Parser ([String], [String])
entry = do
    lhs <- (:) <$> some letterChar <*> replicateM 9 (" " *> some letterChar)
    " | "
    rhs <- (:) <$> some letterChar <*> replicateM 3 (" " *> some letterChar)
    return (lhs, rhs)

isEasyDigit :: String -> Bool
isEasyDigit d = case length d of
    2 -> True  -- 1
    4 -> True  -- 4
    3 -> True  -- 7
    7 -> True  -- 8
    _ -> False

easyDigitCount :: [String] -> Int
easyDigitCount ds = length $ filter isEasyDigit ds

solve1 :: [([String], [String])] -> Int
solve1 entries = sum $ map (easyDigitCount . snd) entries

overlap :: Set Char -> Set Char -> Int
overlap d1 d2 = length $ d1 `Set.intersection` d2

justFind :: (a -> Bool) -> [a] -> a
justFind p = fromJust . find p

deduceDecoder :: [String] -> Map (Set Char) Int
deduceDecoder ds =
    Map.fromList
        [ (zero , 0)
        , (one  , 1)
        , (two  , 2)
        , (three, 3)
        , (four , 4)
        , (five , 5)
        , (six  , 6)
        , (seven, 7)
        , (eight, 8)
        , (nine , 9)
        ]
  where
    ds' = map Set.fromList ds

    zero  = justFind (\d -> length d == 6 && d /= six && d /= nine) ds'
    one   = justFind (\d -> length d == 2) ds'
    two   = justFind (\d -> length d == 5 && overlap four d == 2) ds'
    three = justFind (\d -> length d == 5 && overlap one d == 2) ds'
    four  = justFind (\d -> length d == 4) ds'
    five  = justFind (\d -> length d == 5 && overlap two d == 3) ds'
    six   = justFind (\d -> length d == 6 && overlap one d == 1) ds'
    seven = justFind (\d -> length d == 3) ds'
    eight = justFind (\d -> length d == 7) ds'
    nine  = justFind (\d -> length d == 6 && overlap three d == 5) ds'

decodeDigit :: Map (Set Char) Int -> String -> Int
decodeDigit decoder d = fromJust $ Map.lookup (Set.fromList d) decoder

decodeOutput :: Map (Set Char) Int -> [String] -> Int
decodeOutput decoder ds = read $ concatMap (show . decodeDigit decoder) ds

solve2 :: [([String], [String])] -> Int
solve2 entries = sum $ map (\(patterns, output) -> decodeOutput (deduceDecoder patterns) output) entries

main :: IO ()
main = do
    input <- map (justParse entry) . lines <$> readFile "AOC2021/Day8/input.txt"
    print $ solve1 input
    print $ solve2 input
