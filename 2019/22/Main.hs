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
import Data.FiniteField.PrimeField
import GHC.TypeNats
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

data Shuffle p = Shuffle { multiplyBy :: PrimeField p, thenAdd :: PrimeField p }
    deriving (Show, Eq, Ord)
instance KnownNat p => Semigroup (Shuffle p) where
    (Shuffle a b) <> (Shuffle c d) =
        Shuffle (a * c) (b * c + d)
instance KnownNat p => Monoid (Shuffle p) where
    mempty = Shuffle 1 0

lookupCard :: KnownNat p => PrimeField p -> Shuffle p -> PrimeField p
lookupCard index (Shuffle {..}) = index * multiplyBy + thenAdd

lookupIndex :: KnownNat p => PrimeField p -> Shuffle p -> PrimeField p
lookupIndex card (Shuffle {..}) = (card - thenAdd) / multiplyBy

toShuffle :: KnownNat p => Technique -> Shuffle p
toShuffle DINS = Shuffle (-1) (-1)
toShuffle (C n) = Shuffle 1 (fromIntegral (-n))
toShuffle (DWI n) = Shuffle (fromIntegral n) 0


a :: IO (PrimeField 10007)
a = do
    shuffles <- map toShuffle <$> readInput "input.txt"
    return $ lookupCard 2019 $ mconcat shuffles

b :: IO (PrimeField 119315717514047)
b = do
    shuffles <- map toShuffle <$> readInput "input.txt"
    return $ lookupIndex 2020 $ stimes 101741582076661 $ mconcat shuffles

main :: IO ()
main = do
    a >>= print
    b >>= print
