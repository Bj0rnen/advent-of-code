module AOC2021.Day4.Main where

import AOC2021.Common (justParse, commaSeparatedDecimals, Parser, listMatrix2dU)
import Control.Applicative (Alternative(many))
import Data.Array.Unboxed (listArray, range, UArray, IArray, assocs, (//), (!))
import Data.Maybe (listToMaybe)
import Data.List (find, delete, (\\))

newtype Board = MkBoard (UArray (Int, Int) Int)
    deriving (Show, Eq)

parseBoards :: [String] -> [Board]
parseBoards [] = []
parseBoards (_ : l1 : l2 : l3 : l4 : l5 : ls) =
    MkBoard (listMatrix2dU 5 5 (map (map read . words) [l1, l2, l3, l4, l5])) : parseBoards ls


newtype Mask = MkMask (UArray (Int, Int) Bool)
    deriving (Show, Eq)

emptyMask :: Mask
emptyMask = MkMask (listMatrix2dU 5 5 (replicate 5 (replicate 5 False)))

markPosition :: (Int, Int) -> Mask -> Mask
markPosition position (MkMask mask) = MkMask $ mask // [(position, True)]

hasWon :: Mask -> Bool
hasWon (MkMask mask) = any (all (== True)) ([rowAsList row | row <- [0..4]] ++ [columnAsList col | col <- [0..4]])
  where
    rowAsList row = [mask ! (col, row) | col <- [0..4]]
    columnAsList col = [mask ! (col, row) | row <- [0..4]]

sumUnmarked :: Board -> Mask -> Int
sumUnmarked (MkBoard board) (MkMask mask) = sum unmarkedNumbers
  where
    unmarkedPositions = map fst $ filter (not . snd) (assocs mask)
    unmarkedNumbers = [number | (position, number) <- assocs board, position `elem` unmarkedPositions]


markNumber :: Board -> Mask -> Int -> Mask
markNumber (MkBoard board) mask number =
    case find (\(_, n) -> n == number) (assocs board) of
        Nothing -> mask
        Just (position, _) -> markPosition position mask

winnerScores :: [Int] -> [Board] -> [Int]
winnerScores draw boards = go draw (zip boards (repeat emptyMask))
  where
    go [] maskedBoards = []
    go (number : numbers) maskedBoards =
        map (\(board, mask) -> score board mask number) winners
        ++ go numbers (newMaskedBoards \\ winners)
      where
        newMaskedBoards = map (\(board, mask) -> (board, markNumber board mask number)) maskedBoards
        winners = filter (\(_, mask) -> hasWon mask) newMaskedBoards

score :: Board -> Mask -> Int -> Int
score board mask number = sumUnmarked board mask * number

solve1 :: [Int] -> [Board] -> Int
solve1 draw boards = head $ winnerScores draw boards

solve2 :: [Int] -> [Board] -> Int
solve2 draw boards = last $ winnerScores draw boards

main :: IO ()
main = do
    drawStr : boardLines <- lines <$> readFile "AOC2021/Day4/input.txt"
    let draw = justParse commaSeparatedDecimals drawStr
    let boards = parseBoards boardLines
    print $ solve1 draw boards
    print $ solve2 draw boards
