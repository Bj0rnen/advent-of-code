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


data Shuffle a = Shuffle
    { multiplyBy :: a
    , thenAdd    :: a
    } deriving (Show, Eq, Ord)

instance Num a => Semigroup (Shuffle a) where
    (Shuffle a b) <> (Shuffle c d) =
        Shuffle (a * c) (b * c + d)
instance Num a => Monoid (Shuffle a) where
    mempty = Shuffle 1 0

dealIntoNewStack    = Shuffle (-1) (-1)
cut               n = Shuffle 1    (-n)
dealWithIncrement n = Shuffle n    0


parseTechnique :: Num a => Parser (Shuffle a)
parseTechnique = do
    dealIntoNewStack <$ string "deal into new stack"
    <|> cut . fromIntegral <$> (string "cut " *> L.signed (return ()) L.decimal)
    <|> dealWithIncrement . fromIntegral <$> (string "deal with increment " *> L.decimal)

readInput :: Num a => String -> IO [Shuffle a]
readInput file = map (justParse parseTechnique) . lines <$> readFile file


findCard :: Num a => a -> Shuffle a -> a
findCard card (Shuffle {..}) = card * multiplyBy + thenAdd

lookupIndex :: Fractional a => a -> Shuffle a -> a
lookupIndex index (Shuffle {..}) = (index - thenAdd) / multiplyBy


a :: IO (PrimeField 10007)
a = do
    shuffles <- readInput "input.txt"
    return $ findCard 2019 $ mconcat shuffles

b :: IO Rational
b = do
    shuffles <- readInput "input.txt"
    return $ lookupIndex 2020 $ stimes 101741582076661 $ mconcat shuffles

main :: IO ()
main = do
    a >>= print
    b >>= print
