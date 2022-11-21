module AOC2021.Day1.Main where

countIncreases :: Ord a => [a] -> Int
countIncreases xs = length $ filter (uncurry (<)) $ zip xs (tail xs)

solve1 :: [Int] -> Int
solve1 = countIncreases

solve2 :: [Int] -> Int
solve2 xs =
    countIncreases $ zipWith3 (\x1 x2 x3 -> x1 + x2 + x3) xs (tail xs) (tail (tail xs))

main :: IO ()
main = do
    input <- map read . lines <$> readFile "AOC2021/Day1/input.txt"
    print $ solve1 input
    print $ solve2 input
