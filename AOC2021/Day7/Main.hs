module AOC2021.Day7.Main where

import AOC2021.Common (justParse, commaSeparatedDecimals)

fuelCost1 :: [Int] -> Int -> Int
fuelCost1 xs i = sum $ map (abs . (i -)) xs

fuelCost2 :: [Int] -> Int -> Int
fuelCost2 xs i = sum $ map (\x -> let y = abs (i-x) in (y+1) * y `div` 2) xs

solve :: ([Int] -> Int -> Int) -> [Int] -> Int
solve fuelCost xs = minimum $ map (fuelCost xs) [lowerBound..upperBound]
  where
    lowerBound = minimum xs
    upperBound = maximum xs

solve1 :: [Int] -> Int
solve1 = solve fuelCost1

solve2 :: [Int] -> Int
solve2 = solve fuelCost2

main :: IO ()
main = do
    input <- justParse commaSeparatedDecimals <$> readFile "AOC2021/Day7/input.txt"
    print $ solve1 input
    print $ solve2 input
