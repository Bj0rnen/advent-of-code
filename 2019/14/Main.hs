{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

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

parseMaterial :: Parser (Int, String)
parseMaterial = do
    quantity <- L.signed (return ()) L.decimal <* space1
    chemical <- many alphaNumChar
    pure (quantity, chemical)

parseInputMaterials1 :: Parser [(Int, String)]
parseInputMaterials1 = do
    material <- parseMaterial
    ((material :) <$> (char ',' *> space1 *> parseInputMaterials1))
        <|> space1 $> [material]

parseFormula :: Parser ((Int, String), [(Int, String)])
parseFormula = do
    inputMaterials <- parseInputMaterials1
    string "=>" <* space1
    outputMaterial <- parseMaterial
    eof
    return (outputMaterial, inputMaterials)

makeNanofactory :: [String] -> Map String (Int, [(Int, String)])
makeNanofactory ls =
    Map.fromList $ map (\l -> let ((n, o), is) = justParse parseFormula l in (o, (n, is))) ls

readInput :: String -> IO (Map String (Int, [(Int, String)]))
readInput file = do
    s <- readFile file
    return $ makeNanofactory $ lines s

mergeWaste :: [(Int, String)] -> [(Int, String)]
mergeWaste [] = []
mergeWaste ((n, x) : l) =
    let (xs, ys) = partition ((== x) . snd) l
        total = n + sum (map fst xs)
    in  if total == 0 then
            mergeWaste ys
        else
            (total, x) : mergeWaste ys

mergeWaste' :: (Int, String) -> [[(Int, String)]] -> [(Int, String)]
mergeWaste' w wss = mergeWaste (w : concat wss)

greedyOre :: Map String (Int, [(Int, String)]) -> Int -> String -> (Int, [(Int, String)])
greedyOre nf n "ORE" = (n, [])
greedyOre nf n x =
    let (n', is) = nf Map.! x
        reps = (n + n' - 1) `div` n'  -- Rounds up
        subs = map (\(m, i) -> greedyOre nf (reps * m) i) is
    in  (sum (map fst subs), mergeWaste' (reps * n' - n, x) (map snd subs))

recycle :: Map String (Int, [(Int, String)]) -> (Int, String) -> (Int, [(Int, String)])
recycle nf (n, "ORE") = (n, [])
recycle nf (n, x) =
    let (n', is) = nf Map.! x
        reps = n `div` n'  -- Rounds down
        subs = map (\(m, i) -> recycle nf ((reps * m), i)) is
    in  (sum (map fst subs), mergeWaste' (n - reps * n', x) (map snd subs))

recycleFully :: Map String (Int, [(Int, String)]) -> [(Int, String)] -> Int
recycleFully nf waste =
    let recycled = map (recycle nf) waste
        ore = sum $ map fst recycled
        moreWaste = mergeWaste $ concat (map snd recycled)
    in  if ore == 0 then
            0
        else
            ore + recycleFully nf moreWaste

oreForFuel :: Map String (Int, [(Int, String)]) -> Int -> Int
oreForFuel nf fuel =
    let (ore, waste) = greedyOre nf fuel "FUEL"
        recycledOre = recycleFully nf waste
    in  ore - recycledOre

-- If strictly less than tree element, go left, otherwise right.
data Tree a = Tree Int a (Tree a) (Tree a) | Leaf Int a
    deriving (Show)
-- Only defined for powers of 2
mkTree :: (Int -> a) -> Int -> Tree a
mkTree f size = go f size 0
    where
        go f 1 low = Leaf low (f low)
        go f size low =
            Tree mid (f mid)
                (go f (size `div` 2) low)
                (go f (size `div` 2) mid)
            where
                mid = (size `div` 2) + low
findTreeLE :: Ord a => a -> Tree a -> (Int, a)
findTreeLE _ (Leaf n x) = (n, x)
findTreeLE x (Tree n y l r) =
    if x < y then
        findTreeLE x l
    else
        findTreeLE x r

fuelFromOre :: Map String (Int, [(Int, String)]) -> Int -> Int
fuelFromOre nf ore =
    let -- Assumption: We won't get more fuel than we had ore
        upperBoundPowerOf2 = fromJust $ find (>= ore) [2^i | i <- [0..]]
        searchTree = mkTree (oreForFuel nf) upperBoundPowerOf2
    in  fst $ findTreeLE ore searchTree

a :: IO Int
a = do
    nanofactory <- readInput "input.txt"
    return $ oreForFuel nanofactory 1

b :: IO Int
b = do
    nanofactory <- readInput "input.txt"
    return $ fuelFromOre nanofactory 1000000000000

main :: IO ()
main = do
    a >>= print
    b >>= print
