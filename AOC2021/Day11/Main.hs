module AOC2021.Day11.Main where

import AOC2021.Common (listMatrix2dU, neighbors8, iterateUntilUnchanged, Matrix, MatrixBounds)
import Data.Array.Unboxed (Ix (range), accum, array, (!), elems, IArray (bounds))
import Data.Maybe (fromJust)
import Data.List (findIndex)
import Control.Monad (replicateM)
import Control.Monad.State.Strict (MonadState(state), evalState, State)
import Data.Char (digitToInt)

incrementBy1 :: MatrixBounds -> Matrix Int -> Matrix Int
incrementBy1 bnds m = accum (+) m [(coord, 1) | coord <- range bnds]

flashRound :: MatrixBounds -> Matrix Int -> Matrix Int
flashRound bnds m =
    array bnds [(coord, flash coord) | coord <- range bnds]
  where
    flash coord
        | m ! coord > 9 = 0
        | m ! coord == 0 = 0
        | otherwise =
            (m ! coord) +
            (length $ filter (> 9) $ map (m !) $ neighbors8 bnds coord)

step :: MatrixBounds -> Matrix Int -> Matrix Int
step bnds m = iterateUntilUnchanged (flashRound bnds) (incrementBy1 bnds m)

flashCountedStep :: MatrixBounds -> State (Matrix Int) Int
flashCountedStep bnds =
    state $ \m ->
        let m' = step bnds m
            zeroCount = length $ filter (== 0) $ elems m'
        in (zeroCount, m')

solve1 :: Matrix Int -> Int
solve1 m = sum $ evalState (replicateM 100 (flashCountedStep bnds)) m
  where
    bnds = bounds m

allZeroes :: MatrixBounds -> Matrix Int -> Bool
allZeroes bnds m = all (== 0) $ elems m

solve2 :: Matrix Int -> Int
solve2 m = fromJust $ findIndex (allZeroes bnds) iterations
  where
    bnds = bounds m
    iterations = iterate (step bnds) m

main :: IO ()
main = do
    input <- listMatrix2dU 10 10 . map (map digitToInt) . lines <$> readFile "AOC2021/Day11/input.txt"
    print $ solve1 input
    print $ solve2 input
