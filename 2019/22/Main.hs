{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Maybe
import Data.Semigroup
import Data.FiniteField
import GHC.TypeNats

type Parser = Parsec Void String

justParse :: (Stream s, Ord e) => Parsec e s a -> s -> a
justParse p s = fromJust $ parseMaybe p s


data Shuffle p = Shuffle
    { multiplyBy :: PrimeField p
    , thenAdd :: PrimeField p
    } deriving (Show, Eq, Ord)

instance KnownNat p => Semigroup (Shuffle p) where
    (Shuffle a b) <> (Shuffle c d) =
        Shuffle (a * c) (b * c + d)
instance KnownNat p => Monoid (Shuffle p) where
    mempty = Shuffle 1 0

dealIntoNewStack    = Shuffle (-1) (-1)
cut               n = Shuffle 1    (-n)
dealWithIncrement n = Shuffle n    0


parseTechnique :: KnownNat p => Parser (Shuffle p)
parseTechnique = do
    dealIntoNewStack <$ string "deal into new stack"
    <|> cut . fromIntegral <$> (string "cut " *> L.signed (return ()) L.decimal)
    <|> dealWithIncrement . fromIntegral <$> (string "deal with increment " *> L.decimal)

readInput :: KnownNat p => String -> IO [Shuffle p]
readInput file = map (justParse parseTechnique) . lines <$> readFile file


lookupCard :: KnownNat p => PrimeField p -> Shuffle p -> PrimeField p
lookupCard index (Shuffle {..}) = index * multiplyBy + thenAdd

lookupIndex :: KnownNat p => PrimeField p -> Shuffle p -> PrimeField p
lookupIndex card (Shuffle {..}) = (card - thenAdd) / multiplyBy


a :: IO (PrimeField 10007)
a = do
    shuffles <- readInput "input.txt"
    return $ lookupCard 2019 $ mconcat shuffles

b :: IO (PrimeField 119315717514047)
b = do
    shuffles <- readInput "input.txt"
    return $ lookupIndex 2020 $ stimes 101741582076661 $ mconcat shuffles

main :: IO ()
main = do
    a >>= print
    b >>= print
