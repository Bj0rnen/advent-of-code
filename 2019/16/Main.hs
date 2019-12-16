{-# LANGUAGE TypeApplications #-}

import Data.Char
import Control.DeepSeq
import Data.List
import Data.Array
import Debug.Trace

onesDigit :: Int -> Int
onesDigit i = abs i `mod` 10

phase :: [Int] -> [Int]
phase signal =
    signal `deepseq`
        [onesDigit $ sum $ zipWith (*) (tail $ concatMap (replicate i) (cycle [0, 1, 0, -1])) signal | i <- [1..length signal]]

a :: IO [Char]
a = do
    [signal] <- map (map digitToInt) . lines <$> readFile "input.txt"
    let phases = iterate phase signal
    return $ take 8 $ map intToDigit $ phases !! 100

b :: IO [Char]
b = do
    [signal] <- map (map digitToInt) . lines <$> readFile "input.txt"
    let realSignal = concat $ replicate 10000 signal
        offset = read @Int $ map intToDigit $ take 7 signal

    --putStrLn "length realSignal:"
    --print $ length realSignal
    --putStrLn "offset:"
    --print offset
    
    -- I've concluded that offset is beyond half of mid-point of realSignal.
    -- Soo...
    let significantPart = drop offset realSignal
        size = length significantPart
        bnds = (0, size-1)
        arr 0 = listArray bnds significantPart
        arr n =
            let prev = arr (n - 1)
                curr =
                    deepseq prev $ --traceShow n $
                        listArray bnds
                            [if i == size - 1 then
                                prev ! i
                             else
                                onesDigit ((prev ! i) + (curr ! (i + 1)))
                            | i <- range bnds]
            in  curr
        arr100 = arr 100
    return $ take 8 $ map intToDigit $ elems arr100

main :: IO ()
main = do
    a >>= putStrLn
    b >>= putStrLn
