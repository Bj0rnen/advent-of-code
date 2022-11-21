{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module AOC2021.Common where

import Text.Megaparsec ( parseMaybe, Parsec, Stream (Token), MonadParsec (eof), parse )
import Data.Void ( Void )
import Data.Maybe (fromJust, catMaybes)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (char, newline, eol)
import Control.Applicative (Alternative((<|>)))
import Data.Functor (void, ($>))
import Data.Array.Unboxed (UArray, IArray (bounds), listArray, Ix (inRange), (!), array, assocs)
import Data.List (find, transpose)

type Parser = Parsec Void String

justParse :: (Show s, Show (Token s), Show e) => (Stream s, Ord e) => Parsec e s a -> s -> a
justParse p s =
    case parse p "" s of
        Left err -> error (show err)
        Right x -> x

commaSeparated :: Parser a -> Parser [a]
commaSeparated p = do
    n <- p
    ns <- char ',' *> commaSeparated p
      <|> (eof <|> void newline) $> []
    return (n : ns)

commaSeparatedDecimals :: Parser [Int]
commaSeparatedDecimals = commaSeparated L.decimal

int :: Parser Int
int = L.signed (pure ()) L.decimal


mapUArray :: (Ix i, IArray UArray a, IArray UArray b) => (a -> b) -> UArray i a -> UArray i b
mapUArray f arr = array (bounds arr) [(i, f x) | (i, x) <- assocs arr]

type Matrix = UArray (Int, Int)
type MatrixBounds = ((Int, Int), (Int, Int))

listMatrix2dU :: IArray UArray a => Int -> Int -> [[a]] -> Matrix a
listMatrix2dU width height xss =
    listArray ((0, 0), (width - 1, height - 1)) (concat (transpose xss))

listMatrix2dUInferDimensions :: IArray UArray a => [[a]] -> Matrix a
listMatrix2dUInferDimensions xss = listMatrix2dU width height xss
  where
    width = length $ head xss
    height = length xss

neighbors4 :: MatrixBounds -> (Int, Int) -> [(Int, Int)]
neighbors4 bnds (x, y) =
    filter (inRange bnds)
        [                 (x + 0, y - 1)
        , (x - 1, y + 0)                , (x + 1, y + 0)
                        , (x + 0, y + 1)
        ]

neighbors8 :: MatrixBounds -> (Int, Int) -> [(Int, Int)]
neighbors8 bnds (x, y) =
    filter (inRange bnds)
        [ (x - 1, y - 1), (x + 0, y - 1), (x + 1, y - 1)
        , (x - 1, y + 0)                , (x + 1, y + 0)
        , (x - 1, y + 1), (x + 0, y + 1), (x + 1, y + 1)
        ]

showMatrix :: (IArray UArray a, Show a) => Matrix a -> String
showMatrix m =
    unlines ("[" : [' ' : show [m ! (x, y) | x <- [0 .. mx]] | y <- [0 .. my]]) ++ "]"
  where
    ((_, _), (mx, my)) = bounds m

printMatrix :: (IArray UArray a, Show a) => Matrix a -> IO ()
printMatrix = putStrLn . showMatrix

showCharMatrix :: Matrix Char -> String
showCharMatrix m =
    unlines [[m ! (x, y) | x <- [0 .. mx]] | y <- [0 .. my]]
  where
    ((_, _), (mx, my)) = bounds m

printCharMatrix :: Matrix Char -> IO ()
printCharMatrix = putStrLn . showCharMatrix


firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing : xs) = firstJust xs
firstJust (Just x : xs) = Just x

justFirstJust :: [Maybe a] -> a
justFirstJust (Nothing : xs) = justFirstJust xs
justFirstJust (Just x : xs) = x

firstByMaybe :: (a -> Maybe b) -> [a] -> Maybe (a, b)
firstByMaybe f xs = firstJust $ map (\x -> fmap (x,) (f x)) xs

justFirstByMaybe :: (a -> Maybe b) -> [a] -> (a, b)
justFirstByMaybe f = fromJust . firstByMaybe f

justIfEqual :: Eq a => a -> a -> Maybe a
justIfEqual x y = if x == y then Just x else Nothing

iterateUntilUnchanged :: Eq a => (a -> a) -> a -> a
iterateUntilUnchanged f !x =
    justFirstJust $ zipWith justIfEqual iterations (tail iterations)
  where
    iterations = iterate f x

iterateUntilUnchangedWithIndex :: Eq a => (a -> a) -> a -> (a, Int)
iterateUntilUnchangedWithIndex f !x =
    justFirstJust $ zipWith (\i x -> fmap (,i) x) [0..] $ zipWith justIfEqual iterations (tail iterations)
  where
    iterations = iterate f x
