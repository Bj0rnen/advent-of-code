{-# LANGUAGE LambdaCase #-}

module AOC2021.Day3.Main where

import Data.List (transpose)
import Data.Char (digitToInt)
import Data.Bits (Bits(shiftL, (.|.)))
import GHC.List (foldl')

mostCommonBit :: String -> Char
mostCommonBit bits
  | zeros > ones = '0'
  | otherwise = '1'
  where
    (zeros, ones) = foldl' (\(zs, os) -> \case '0' -> (zs + 1, os); '1' -> (zs, os + 1)) (0, 0) bits

negateBit :: Char -> Char 
negateBit '0' = '1'
negateBit '1' = '0'

binaryToInt :: String -> Int
binaryToInt bits = foldl' (\i d -> (i `shiftL` 1) .|. d) 0 digits
  where
    digits = map digitToInt bits

solve1 :: [String] -> Int
solve1 xss = gamma * epsilon
  where
    txss = transpose xss
    mcbs = map mostCommonBit txss
    gamma = binaryToInt mcbs
    epsilon = binaryToInt (map negateBit mcbs)

rating :: Bool -> [String] -> String
rating _ [xs] = xs
rating mostCommonWins xss = winningBit : rating mostCommonWins xss'
  where
    txs = map head xss
    mcb = mostCommonBit txs
    winningBit = if mostCommonWins then mcb else negateBit mcb
    xss' = map tail $ filter (\xs -> head xs == winningBit) xss

solve2 :: [String] -> Int
solve2 xss = oxygenGenerator * co2scrubber
  where
    oxygenGenerator = binaryToInt $ rating True xss
    co2scrubber = binaryToInt $ rating False xss

main :: IO ()
main = do
    input <- lines <$> readFile "AOC2021/Day3/input.txt"
    print $ solve1 input
    print $ solve2 input
