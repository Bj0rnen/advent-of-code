module AOC2021.Day15.Main where

import AOC2021.Common (Parser, justParse, listMatrix2dUInferDimensions, Matrix, printMatrix, neighbors4, iterateUntilUnchanged)
import Data.Char (digitToInt)
import qualified Data.Array.Unboxed as U
import Data.Array (array, (!), Ix (range), Array)

onlyRightAndDown :: Matrix Int -> Matrix Int
onlyRightAndDown m = U.array bnds [(coord, riskLevels ! coord) | coord <- range bnds]
  where
    bnds@(_, (maxX, maxY)) = U.bounds m
    riskLevels = array bnds [(coord, riskLevel coord) | coord <- range bnds]
    riskLevel (x, y)
      | x == maxX && y == maxY = 0
      | x == maxX = m U.! (x    , y + 1) + riskLevels ! (x    , y + 1)
      | y == maxY = m U.! (x + 1, y    ) + riskLevels ! (x + 1, y    )
      | otherwise =
        min
            (m U.! (x + 1, y    ) + riskLevels ! (x + 1, y    ))
            (m U.! (x    , y + 1) + riskLevels ! (x    , y + 1))

improve :: Matrix Int -> Matrix Int -> Matrix Int
improve m riskLevels = riskLevels'
  where
    bnds@(_, (maxX, maxY)) = U.bounds riskLevels
    improvedRiskLevel (x, y)
      | x == maxX && y == maxY = riskLevels U.! (x, y)
      | otherwise =
        minimum $ map (\coord -> m U.! coord + riskLevels U.! coord) $ neighbors4 bnds (x, y)
    riskLevels' = U.array bnds [(coord, improvedRiskLevel coord) | coord <- range bnds] :: Matrix Int

minimumRiskLevel :: Matrix Int -> Int
minimumRiskLevel m = riskLevels U.! (0, 0)
  where
    firstGen = onlyRightAndDown m
    riskLevels = iterateUntilUnchanged (improve m) firstGen

solve1 :: Matrix Int -> Int
solve1 = minimumRiskLevel

fullMap :: Matrix Int -> Matrix Int
fullMap m =
    U.array bnds'
        [ (coord, ((((m U.! (x `mod` width, y `mod` height)) + (x `div` width) + (y `div` height)) - 1) `mod` 9) + 1)
        | coord@(x, y) <- range bnds']
  where
    bnds@(_, (maxX, maxY)) = U.bounds m
    width  = maxX + 1
    height = maxY + 1
    bnds' = ((0, 0), ((width * 5) - 1, (height * 5) - 1))

solve2 :: Matrix Int -> Int
solve2 m = minimumRiskLevel (fullMap m)

main :: IO ()
main = do
    m <- listMatrix2dUInferDimensions . map (map digitToInt) . lines <$> readFile "AOC2021/Day15/input.txt"
    print $ solve1 m
    print $ solve2 m
