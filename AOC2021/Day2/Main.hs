{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module AOC2021.Day2.Main where

import AOC2021.Common ( Parser, justParse )

import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative (Alternative((<|>)))
import Data.List (foldl')

data Position1 = MkPosition1 { horizontal :: !Int, depth :: !Int }

data Position2 = MkPosition2 { horizontal :: !Int, depth :: !Int, aim :: !Int }

data Direction = Forward | Down | Up

data Command = MkCommand { direction :: !Direction, distance :: !Int }

parseCommand :: Parser Command
parseCommand = do
    direction <- Forward <$ "forward" <|> Up <$ "up" <|> Down <$ "down"
    " "
    distance <- L.decimal
    return $ MkCommand { direction = direction, distance = distance }

move1 :: Command -> Position1 -> Position1
move1 (MkCommand Forward dist) (MkPosition1 x y) = MkPosition1 (x + dist) y
move1 (MkCommand Down dist) (MkPosition1 x y) = MkPosition1 x (y + dist)
move1 (MkCommand Up dist) (MkPosition1 x y) = MkPosition1 x (y - dist)

move2 :: Command -> Position2 -> Position2
move2 (MkCommand Forward dist) (MkPosition2 x y a) = MkPosition2 (x + dist) (y + a * dist) a
move2 (MkCommand Down dist) (MkPosition2 x y a) = MkPosition2 x y (a + dist)
move2 (MkCommand Up dist) (MkPosition2 x y a) = MkPosition2 x y (a - dist)

solve1 :: [Command] -> Int
solve1 cmds = x * y
  where
    (MkPosition1 x y) = foldl' (flip move1) (MkPosition1 0 0) cmds

solve2 :: [Command] -> Int
solve2 cmds = x * y
  where
    (MkPosition2 x y a) = foldl' (flip move2) (MkPosition2 0 0 0) cmds

main :: IO ()
main = do
    input <- map (justParse parseCommand) . lines <$> readFile "AOC2021/Day2/input.txt"
    print $ solve1 input
    print $ solve2 input
