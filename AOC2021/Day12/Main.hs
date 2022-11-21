{-# LANGUAGE OverloadedStrings #-}

module AOC2021.Day12.Main where

import AOC2021.Common (justParse, Parser)
import Control.Applicative (Alternative(some, (<|>)))
import Text.Megaparsec.Char (letterChar, lowerChar, upperChar)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Control.Monad (guard)

data Cave = Small String | Big String
    deriving (Show, Eq, Ord)

cave :: Parser Cave
cave = Small <$> some lowerChar <|> Big <$> some upperChar

entry :: Parser (Cave, Cave)
entry = do
    c1 <- cave
    "-"
    c2 <- cave
    return (c1, c2)

data CaveNode = MkCaveNode Cave [CaveNode]
    deriving (Show)

justLookup :: Ord k => k -> Map k v -> v
justLookup k = fromJust . Map.lookup k

caveSystem :: [(Cave, Cave)] -> CaveNode
caveSystem connections = justLookup (Small "start") caveNodes
  where
    withFlipped = connections ++ map (\(x, y) -> (y, x)) connections
    caves = Set.fromList $ map fst withFlipped
    caveNodes =
        Map.fromList
            [ (cave, MkCaveNode cave (map (\(_, y) -> justLookup y caveNodes) $ filter (\(x, y) -> x == cave) withFlipped))
            | cave <- Set.toList caves
            ]

allPaths1 :: Set.Set Cave -> CaveNode -> [[Cave]]
allPaths1 visited (MkCaveNode end@(Small "end") neighbors) = [[end]]
allPaths1 visited (MkCaveNode node neighbors) = do
    neighbor@(MkCaveNode cave _) <- neighbors
    guard $ Set.notMember cave visited
    let visited' = case node of
            Small n -> Set.insert (Small n) visited
            Big _ -> visited
    path <- allPaths1 visited' neighbor
    return $ node : path

solve1 :: CaveNode -> Int
solve1 startNode = length (allPaths1 Set.empty startNode)

allPaths2 :: Set.Set Cave -> Bool -> CaveNode -> [[Cave]]
allPaths2 visited hasDoubleVisited (MkCaveNode end@(Small "end") neighbors) = [[end]]
allPaths2 visited hasDoubleVisited (MkCaveNode node neighbors) = do
    neighbor@(MkCaveNode cave _) <- neighbors
    guard $ cave /= Small "start"
    guard $ Set.notMember cave visited || not hasDoubleVisited
    let visited' = case node of
            Small n -> Set.insert (Small n) visited
            Big _ -> visited
    let hasDoubleVisited' = hasDoubleVisited || Set.member cave visited
    path <- allPaths2 visited' hasDoubleVisited' neighbor
    return $ node : path

solve2 :: CaveNode -> Int
solve2 startNode = length (allPaths2 Set.empty False startNode)

main :: IO ()
main = do
    startNode <- caveSystem . map (justParse entry) . lines <$> readFile "AOC2021/Day12/input.txt"
    print $ solve1 startNode
    print $ solve2 startNode
