{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Data.Semigroup
import Data.Reflection
import Data.FiniteField.PrimeField
import Debug.Trace

type Parser = Parsec Void String

justParse :: (Stream s, Ord e) => Parsec e s a -> s -> a
justParse p s = fromJust $ parseMaybe p s

data Technique =
    DINS
    | C Integer
    | DWI Integer

parseTechnique :: Parser Technique
parseTechnique = do
    DINS <$ string "deal into new stack"
    <|> C <$> (string "cut " *> L.signed (return ()) L.decimal)
    <|> DWI <$> (string "deal with increment " *> L.decimal)

readInput :: String -> IO [Technique]
readInput file = do
    s <- readFile file
    return $ map (justParse parseTechnique) $ lines s

data Shuffle = Shuffle { multiplyBy :: Integer, thenAdd :: Integer }
    deriving (Show, Eq, Ord)
instance Given Integer => Semigroup Shuffle where
    (Shuffle a b) <> (Shuffle c d) =
        Shuffle ((a * c) `mod` given) ((b * c + d) `mod` given)
instance Given Integer => Monoid Shuffle where
    mempty = Shuffle 1 0

lookupCard :: Given Integer => Integer -> Shuffle -> Integer
lookupCard index (Shuffle {..}) = (index * multiplyBy + thenAdd) `mod` given

lookupIndex :: Given Integer => Integer -> Shuffle -> Integer
lookupIndex card (Shuffle {..}) =
    let card' :: $(primeField 119315717514047)
        card' = fromIntegral card
        mult' :: $(primeField 119315717514047)
        mult' = fromIntegral multiplyBy
        add' :: $(primeField 119315717514047)
        add' = fromIntegral thenAdd
    in  Data.FiniteField.PrimeField.toInteger $ (card' - add') / mult'

toShuffle :: Technique -> Shuffle
toShuffle DINS = Shuffle (-1) (-1)
toShuffle (C n) = Shuffle 1 (-n)
toShuffle (DWI n) = Shuffle n 0


a :: IO Integer
a = do
    shuffles <- map toShuffle <$> readInput "input.txt"
    return $ give 10007 $ lookupCard 2019 $ mconcat shuffles

b :: IO Integer
b = do
    shuffles <- map toShuffle <$> readInput "input.txt"
    return $ give 119315717514047 $ lookupIndex 2020 $ stimes 101741582076661 $ mconcat shuffles

main :: IO ()
main = do
    a >>= print
    b >>= print
