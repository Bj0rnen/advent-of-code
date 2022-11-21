{-# LANGUAGE OverloadedStrings #-}

module AOC2021.Day14.Main where

import AOC2021.Common (Parser, justParse)
import Text.Megaparsec.Char (letterChar, eol)
import Text.Megaparsec (manyTill, MonadParsec (eof))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Arrow (Arrow(second))
import Data.List (sort, group, groupBy)
import Data.Function (on)
import Debug.Trace (traceShowId)

rule :: Parser ((Char, Char), Char)
rule = do
    pair <- (,) <$> letterChar <*> letterChar
    " -> "
    inserted <- letterChar
    return (pair, inserted)

manual :: Parser ([Char], [((Char, Char), Char)])
manual = do
    xs <- manyTill letterChar eol
    eol
    rules <- manyTill (rule <* eol) eof
    return (xs, rules)

-- TODO: Move to Common
interleave :: [a] -> [a] -> [a]
interleave [] [] = []
interleave xs [] = xs
interleave [] ys = ys
interleave (x : xs) (y : ys) = x : y : interleave xs ys

step :: Map (Char, Char) Char -> [Char] -> [Char]
step rules xs = interleave xs $ zipWith (curry (rules Map.!)) xs (tail xs)

-- TODO: Move to Common
counts :: Ord a => [a] -> Map a Int
counts xs = Map.fromList $ map (\grp -> (head grp, length grp)) $ group $ sort xs

sumCounts :: Ord a => [(a, Int)] -> Map a Int
sumCounts xs =
    Map.fromList $
    map (\grp -> (fst (head grp), sum (map snd grp))) $
    groupBy ((==) `on` fst) $
    sort xs

solve1 :: [Char] -> Map (Char, Char) Char -> Int
solve1 xs rules = mostCommonElement - leastCommonElement
  where
    finalPolymer = iterate (step rules) xs !! 10
    elementCounts = counts finalPolymer
    mostCommonElement = maximum elementCounts
    leastCommonElement = minimum elementCounts

pairCountsToElementCounts :: Char -> Char -> Map (Char, Char) Int -> Map Char Int
pairCountsToElementCounts firstElement lastElement pairCounts =
    Map.map (`div` 2) $
    Map.adjust (+ 1) firstElement $
    Map.adjust (+ 1) lastElement doubleCounted
  where
    doubleCounted =
        sumCounts [(e, count) | ((e1, e2), count) <- Map.toList pairCounts, e <- [e1, e2]]

computeElementCounts :: Map (Char, Char) ((Char, Char), (Char, Char)) -> Char -> Char -> Map (Char, Char) Int -> Int -> Map Char Int
computeElementCounts _     firstElement lastElement pairCounts 0 = pairCountsToElementCounts firstElement lastElement pairCounts
computeElementCounts rules firstElement lastElement pairCounts n =
    computeElementCounts rules firstElement lastElement pairCounts' (n - 1)
  where
    pairCounts' =
        sumCounts [(p, count) | (pair, count) <- Map.toList pairCounts, p <- let (p1, p2) = rules Map.! pair in [p1, p2]]

solve2 :: [Char] -> Map (Char, Char) Char -> Int
solve2 xs rules = mostCommonElement - leastCommonElement
  where
    toPairRules = Map.mapWithKey (\(e1, e3) e2 -> ((e1, e2), (e2, e3))) rules
    elementCounts = traceShowId $ computeElementCounts toPairRules (head xs) (last xs) (counts (zip xs (tail xs))) 40
    mostCommonElement = maximum $ map snd $ Map.toList elementCounts
    leastCommonElement = minimum $ map snd $ Map.toList elementCounts

main :: IO ()
main = do
    (xs, rules) <- second Map.fromList . justParse manual <$> readFile "AOC2021/Day14/input.txt"
    print $ solve1 xs rules
    print $ solve2 xs rules
