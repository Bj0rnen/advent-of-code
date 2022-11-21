{-# LANGUAGE OverloadedStrings #-}

module AOC2021.Day21.Main where

import AOC2021.Common (Parser, justParse)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (eol)
import Text.Megaparsec (MonadParsec(eof))
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Array (Array, array, Ix (range), (!))
import Data.Biapplicative (Bifunctor(bimap))

startingPosition :: Parser Int
startingPosition = "Player " *> L.decimal *> " starting position: " *> L.decimal

deterministicDie :: [Int]
deterministicDie = cycle [1..100]

type Space = Int
type Score = Int

data GameState = MkGameState
    { die        :: [Int]
    , rolls      :: Int
    , nextPlayer :: (Space, Score)
    , lastPlayer :: (Space, Score)
    } deriving (Show)

move :: Int -> Space -> Space
move steps currentSpace = ((currentSpace - 1 + steps) `mod` 10) + 1

turn :: GameState -> GameState
turn (MkGameState die rolls (nextPlayerSpace, nextPlayerScore) lastPlayer) =
    MkGameState die' (rolls + 3) lastPlayer (lastPlayerSpace', lastPlayerScore')
  where
    (threeRolls, die') = splitAt 3 die
    dieSum = sum threeRolls
    lastPlayerSpace' = move nextPlayerSpace dieSum
    lastPlayerScore' = nextPlayerScore + lastPlayerSpace'

lastPlayerWon :: GameState -> Bool
lastPlayerWon (MkGameState _ _ _ (_, lastPlayerScore)) = lastPlayerScore >= 1000

solve1 :: [Int] -> Int
solve1 [s1, s2] = (snd . nextPlayer) finalState * rolls finalState
  where
    initialState = MkGameState deterministicDie 0 (s1, 0) (s2, 0)
    finalState = fromJust $ find lastPlayerWon $ iterate turn initialState

weighedThreeDiceSums :: [(Int, Int)]
weighedThreeDiceSums = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

universes :: Array ((Int, Int), (Int, Int)) (Int, Int)
universes =
    array bnds [(state, countWinningUniverses state) | state <- range bnds]
  where
    bnds = (((1, 0), (1, 0)), ((10, 20), (10, 30)))
    countWinningUniverses :: ((Int, Int), (Int, Int)) -> (Int, Int)
    countWinningUniverses ((nextPlayerSpace, nextPlayerScore), (lastPlayerSpace, lastPlayerScore))
      | lastPlayerScore >= 21 = (0, 1)
      | otherwise =
        bimap sum sum $ unzip $ do
            (dieSum, universeCount) <- weighedThreeDiceSums
            let lastPlayerSpace' = move nextPlayerSpace dieSum
                lastPlayerScore' = nextPlayerScore + lastPlayerSpace'
                (lastPlayerWinningUniverseCount, nextPlayerWinningUniverseCount) =
                    universes ! ((lastPlayerSpace, lastPlayerScore), (lastPlayerSpace', lastPlayerScore'))
            return (nextPlayerWinningUniverseCount * universeCount, lastPlayerWinningUniverseCount * universeCount)


solve2 :: [Int] -> Int
solve2 [s1, s2] = max u1 u2
  where
    (u1, u2) = universes ! ((s1, 0), (s2, 0))

main :: IO ()
main = do
    input <- map (justParse startingPosition) . lines <$> readFile "AOC2021/Day21/input.txt"
    print $ solve1 input
    print $ solve2 input
