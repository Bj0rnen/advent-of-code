{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module AOC2021.Day20.Main where

import AOC2021.Common (Parser, Matrix, justParse, listMatrix2dUInferDimensions, printMatrix, showCharMatrix, mapUArray)
import Data.Array.Unboxed (UArray, listArray, elems, IArray (bounds), array, Ix (range, inRange), (!))
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (eol)
import Control.Monad (replicateM)
import Text.Megaparsec (manyTill, someTill, MonadParsec (eof))
import Data.List (foldl')
import Data.Bits (Bits(shiftL))

data InfiniteImage = MkInfiniteImage { outside :: !Bool, inside :: !(Matrix Bool) }
    deriving (Show)

charToPixel :: Char -> Bool
charToPixel '.' = False
charToPixel '#' = True

pixelToChar :: Bool -> Char
pixelToChar False = '.'
pixelToChar True  = '#'

showImage :: InfiniteImage -> String
showImage (MkInfiniteImage outside inside) =
    unlines ["outside: " ++ [pixelToChar outside], ""] ++ showCharMatrix (mapUArray pixelToChar inside)

pixel :: Parser Bool
pixel = charToPixel <$> L.charLiteral

algorithm :: Parser (UArray Int Bool)
algorithm =
    listArray (0, 511)  <$> replicateM 512 pixel

imageRegion :: Parser InfiniteImage
imageRegion = do
    lns <- manyTill (someTill pixel eol) eof
    return $ MkInfiniteImage False (listMatrix2dUInferDimensions lns)

inputParser :: Parser (UArray Int Bool, InfiniteImage)
inputParser = do
    algo <- algorithm <* eol
    eol
    img <- imageRegion
    return (algo, img)

square :: (Int, Int) -> [(Int, Int)]
square (x, y) =
    [ (x - 1, y - 1), (x    , y - 1), (x + 1, y - 1)
    , (x - 1, y    ), (x    , y    ), (x + 1, y    )
    , (x - 1, y + 1), (x    , y + 1), (x + 1, y + 1)
    ]

bitsToInt :: [Bool] -> Int
bitsToInt = foldl' (\i b -> (i `shiftL` 1) + if b then 1 else 0) 0

indexAlgorithm :: UArray Int Bool -> Int -> Bool
indexAlgorithm algo i = algo ! i

enhance :: UArray Int Bool -> InfiniteImage -> InfiniteImage
enhance algo !image =
    MkInfiniteImage (not (outside image)) newInside
  where
    bnds@((x0, y0), (x1, y1)) = bounds $ inside image
    bnds' = ((x0 - 1, y0 - 1), (x1 + 1, y1 + 1))
    getBit coord
      | inRange bnds coord = inside image ! coord
      | otherwise = outside image
    derivePixel coord = indexAlgorithm algo $ bitsToInt $ map getBit $ square coord
    newInside = array bnds' [(coord, derivePixel coord) | coord <- range bnds']

solve1 :: UArray Int Bool -> InfiniteImage -> Int
solve1 algo image =
    length $ filter id $ elems $ inside $ iterate (enhance algo) image !! 2

solve2 :: UArray Int Bool -> InfiniteImage -> Int
solve2 algo image =
    length $ filter id $ elems $ inside $ iterate (enhance algo) image !! 50

main :: IO ()
main = do
    (algo, image) <- justParse inputParser <$> readFile "AOC2021/Day20/input.txt"
    --putStrLn $ showImage image
    print $ solve1 algo image
    print $ solve2 algo image
