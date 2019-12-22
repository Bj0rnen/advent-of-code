{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor
import Data.Void
import Data.Maybe
import Data.Array.Unboxed
import Data.Array.MArray
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bifunctor

type Parser = Parsec Void String

justParse :: (Stream s, Ord e) => Parsec e s a -> s -> a
justParse p s = fromJust $ parseMaybe p s

data Technique =
    DINS
    | C Int
    | DWI Int

parseTechnique :: Parser Technique
parseTechnique = do
    DINS <$ string "deal into new stack"
    <|> C <$> (string "cut " *> L.signed (return ()) L.decimal)
    <|> DWI <$> (string "deal with increment " *> L.decimal)

readInput :: String -> IO [Technique]
readInput file = do
    s <- readFile file
    return $ map (justParse parseTechnique) $ lines s

positionOf :: Int -> UArray Int Int -> Int
positionOf i = fromJust . findIndex (== i) . elems

performTechnique :: Int -> Technique -> UArray Int Int -> UArray Int Int
performTechnique size DINS !cards = ixmap (0, size - 1) ((size - 1) -) cards
performTechnique size (C n) !cards = ixmap (0, size - 1) (\i -> (i + n + size) `mod` size) cards
performTechnique size (DWI n) !cards = array (0, size - 1) $ map (\(i, c) -> ((i * n) `mod` size, c)) $ assocs cards

performShuffle :: Int -> [Technique] -> UArray Int Int
performShuffle size shuffle =
    foldl' (flip (performTechnique size)) (listArray (0, size - 1) [0..size - 1]) shuffle

a :: IO Int
a = do
    shuffle <- readInput "input.txt"
    return $ positionOf 2019 $ performShuffle 10007 shuffle

b :: IO Int
b = do
    undefined

main :: IO ()
main = do
    a >>= print
    --b >>= print
