module AOC2021.Day9.Main where

import Data.Array.Unboxed (UArray, Ix (range), IArray (bounds), (!))
import AOC2021.Common (listMatrix2dUInferDimensions, neighbors4)
import Data.Char (digitToInt)
import Control.Monad.ST (runST, ST)
import Data.Array.ST (thaw, STUArray, MArray (getBounds), readArray, writeArray)
import Data.List (sortOn)
import Data.Ord (Down(Down))
import Data.Maybe (catMaybes)

solve1 :: UArray (Int, Int) Char -> Int
solve1 floor = sum $ map ((+ 1) . digitToInt . (floor !)) $ filter isLowPoint (range bnds)
  where
    bnds = bounds floor
    isLowPoint coord =
        let nbrs = neighbors4 bnds coord
        in  all ((> floor ! coord) . (floor !)) nbrs

fillFloor :: ((Int, Int), (Int, Int)) -> STUArray s (Int, Int) Char -> (Int, Int) -> ST s Int
fillFloor bnds floor coord = do
    d <- readArray floor coord
    if d == '9' then
        return 0
    else do
        writeArray floor coord '9'
        spread <- mapM (fillFloor bnds floor) $ neighbors4 bnds coord
        return $ 1 + sum spread

allBasins :: STUArray s (Int, Int) Char -> ST s [Int]
allBasins floor = do
    bnds <- getBounds floor
    filter (/= 0) <$> mapM (\coord -> do fillFloor bnds floor coord) (range bnds)

solve2 :: UArray (Int, Int) Char -> Int
solve2 immutableFloor = runST $ do
    floor <- thaw immutableFloor
    candidates <- allBasins floor
    return $ product $ take 3 $ sortOn Down candidates

main :: IO ()
main = do
    floor <- listMatrix2dUInferDimensions . lines <$> readFile "AOC2021/Day9/input.txt"
    print $ solve1 floor
    print $ solve2 floor
