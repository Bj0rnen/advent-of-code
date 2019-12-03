-- NOTE: Uses about 12 GB of memory... Very brute force :P 

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
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

type Parser = Parsec Void String

justParse :: (Stream s, Ord e) => Parsec e s a -> s -> a
justParse p s = fromJust $ parseMaybe p s

data Direction = U | R | D | L
    deriving (Show)
data Segment = Segment
    { direction :: Direction
    , distance :: Int
    } deriving (Show)
newtype Path = Path { segments :: [Segment] }
    deriving (Show)

parseDirection :: Parser Direction
parseDirection =
        char 'U' *> pure U
    <|> char 'R' *> pure R
    <|> char 'D' *> pure D
    <|> char 'L' *> pure L

parseSegment :: Parser Segment
parseSegment = Segment <$> parseDirection <*> L.decimal

parseSegments :: Parser [Segment]
parseSegments = do
    x <- parseSegment
    xs <-   char ',' *> parseSegments
        <|> eof *> return []
    return (x : xs)

parsePath :: Parser Path
parsePath = Path <$> parseSegments

readInput :: IO (Path, Path)
readInput = do
    s <- readFile "input.txt"
    let [w1, w2] = justParse parsePath <$> lines s
    return (w1, w2)

addDistance :: Direction -> Int -> (Int, Int) -> (Int, Int)
addDistance U dist (x, y) = (x, y-dist)
addDistance R dist (x, y) = (x+dist, y)
addDistance D dist (x, y) = (x, y+dist)
addDistance L dist (x, y) = (x-dist, y)

writeSegment :: MArray arr Bool m => arr (Int, Int) Bool -> (Int, Int) -> Segment -> m (Int, Int)
writeSegment m coord (Segment dir dist) = do
    forM_ [1..dist] \i ->
        writeArray m (addDistance dir i coord) True
    return $ addDistance dir dist coord

maxPotentialDistance :: Path -> Int
maxPotentialDistance (Path p) =
    sum $ map distance p

maxPotentialDistanceBoth :: Path -> Path -> Int
maxPotentialDistanceBoth p1 p2 =
    max (maxPotentialDistance p1) (maxPotentialDistance p2)

plotPath :: MArray arr Bool m => Int -> Path -> m (arr (Int, Int) Bool)
plotPath maxDist path = do
    m <- newArray ((-maxDist, -maxDist), (maxDist, maxDist)) False
    foldM_ (writeSegment m) (0, 0) (segments path)
    return m

plottedPath :: Int -> Path -> UArray (Int, Int) Bool
plottedPath maxDist path = runSTUArray (plotPath maxDist path)

cross :: UArray (Int, Int) Bool -> (Int, Int) -> Direction -> Int -> Int -> [((Int, Int), Int)]
cross m (x, y) U dist totalSteps =
    map (\y' -> ((x, y'), totalSteps + abs (y' - y))) $
        filter (\y' -> m!(x , y')) [y-1, y-2..y-dist]
cross m (x, y) R dist totalSteps =
    map (\x' -> ((x', y), totalSteps + abs (x' - x))) $
        filter (\x' -> m!(x', y )) [x+1, x+2..x+dist]
cross m (x, y) D dist totalSteps =
    map (\y' -> ((x, y'), totalSteps + abs (y' - y))) $
        filter (\y' -> m!(x , y')) [y+1, y+2..y+dist]
cross m (x, y) L dist totalSteps =
    map (\x' -> ((x', y), totalSteps + abs (x' - x))) $
        filter (\x' -> m!(x', y )) [x-1, x-2..x-dist]

crossingsAndStepsTaken :: UArray (Int, Int) Bool -> Path -> [((Int, Int), Int)]
crossingsAndStepsTaken m path = snd $ foldl' followSegment (((0, 0), 0), []) (segments path)
    where
        followSegment ((coord, totalSteps), cs) (Segment dir dist) =
            ( (addDistance dir dist coord, totalSteps + dist)
            , cs ++ cross m coord dir dist totalSteps)

mapToMinSteps :: [((Int, Int), Int)] -> Map (Int, Int) Int
mapToMinSteps = Map.fromListWith min

debugCrossingsStepsAndNearest :: String -> String -> ([((Int, Int), Int)], Int)
debugCrossingsStepsAndNearest s1 s2 =
    let p1 = justParse parsePath s1
        p2 = justParse parsePath s2
        m = plottedPath (maxPotentialDistanceBoth p1 p2) p1
    in  (crossingsAndStepsTaken m p2,
            minimum $ map (\((x, y), _) -> abs x + abs y) $ crossingsAndStepsTaken m p2)

a :: IO Int
a = do
    (p1, p2) <- readInput
    let m = plottedPath (maxPotentialDistanceBoth p1 p2) p1
    return $ minimum $ map (\((x, y), _) -> abs x + abs y) $ crossingsAndStepsTaken m p2

b :: IO Int
b = do
    (p1, p2) <- readInput
    let maxDist = maxPotentialDistanceBoth p1 p2
        m1 = plottedPath maxDist p1
        cs1 = mapToMinSteps $ crossingsAndStepsTaken m1 p2
        m2 = plottedPath maxDist p2
        cs2 = mapToMinSteps $ crossingsAndStepsTaken m2 p1
        combined = Map.unionWith (+) cs1 cs2
    return $ minimum combined

main :: IO ()
main = do
    a >>= print
    b >>= print
