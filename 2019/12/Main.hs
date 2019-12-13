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

data Vector = Vector { x :: Int, y :: Int, z :: Int }
    deriving (Show)

parsePosition :: Parser Vector
parsePosition = do
    string "<x="
    x <- L.signed (return ()) L.decimal
    string ", y="
    y <- L.signed (return ()) L.decimal
    string ", z="
    z <- L.signed (return ()) L.decimal
    char '>'
    eof
    return $ Vector x y z

data Moon = Moon { position :: Vector, velocity :: Vector }
    deriving (Show)

readInput :: String -> IO [Moon]
readInput file = do
    s <- readFile file
    let positions = justParse parsePosition <$> lines s
    return $ zipWith Moon positions (repeat (Vector 0 0 0))

every :: [a] -> [(a, [a])]
every [] = []
every (x : xs) = (x, xs) : map (second ((:) x)) (every xs)

gravitate :: Int -> Int -> Int
gravitate this other
    | other < this    = -1
    | other > this    = 1
    | otherwise = 0

stepMoonAxis :: (Int, Int) -> [(Int, Int)] -> (Int, Int)
stepMoonAxis (x, dx) others =
    (x + dx', dx')
    where
        dx' = dx + sum (map (\(ox, _) -> gravitate x ox) others)

stepAxis :: [(Int, Int)] -> [(Int, Int)]
stepAxis moonAxes =
    map (uncurry stepMoonAxis) (every moonAxes)

--stepMoon :: Moon -> [Moon] -> Moon
--stepMoon (Moon (Vector x y z) (Vector dx dy dz)) others =
--    Moon (Vector (x + dx') (y + dy') (z + dz')) (Vector dx' dy' dz')
--    where
--        dx' = (dx +) $ sum $ map (\(Moon (Vector ox _ _) _) -> gravitate x ox) others
--        dy' = (dy +) $ sum $ map (\(Moon (Vector _ oy _) _) -> gravitate y oy) others
--        dz' = (dz +) $ sum $ map (\(Moon (Vector _ _ oz) _) -> gravitate z oz) others
--
--step :: [Moon] -> [Moon]
--step moons =
--    map (uncurry stepMoon) (every moons)

energy :: Moon -> Int
energy (Moon (Vector x y z) (Vector dx dy dz)) =
    (abs x + abs y + abs z) * (abs dx + abs dy + abs dz)

totalEnergy :: [Moon] -> Int
totalEnergy moons =
    sum $ map energy moons

xAxis :: Moon -> (Int, Int)
xAxis (Moon (Vector x _ _) (Vector dx _ _)) = (x, dx)
yAxis :: Moon -> (Int, Int)
yAxis (Moon (Vector _ y _) (Vector _ dy _)) = (y, dy)
zAxis :: Moon -> (Int, Int)
zAxis (Moon (Vector _ _ z) (Vector _ _ dz)) = (z, dz)
zipMoon :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Moon
zipMoon (x, dx) (y, dy) (z, dz) = Moon (Vector x y z) (Vector dx dy dz)

loopStartAndLength :: Ord a => [a] -> (Int, Int)
loopStartAndLength xs =
    let (firstInLoop, lengthBeforeLoopback) = go xs Set.empty
        lengthBeforeLoopStart = length $ takeWhile (/= firstInLoop) xs
    in  (lengthBeforeLoopStart, lengthBeforeLoopback - lengthBeforeLoopStart)
    where
        go (x : xs) s =
            if Set.member x s then
                (x, length s)
            else
                go xs (Set.insert x s)

a :: IO Int
a = do
    moons <- readInput "input.txt"
    let xStates = iterate stepAxis $ map xAxis moons
        yStates = iterate stepAxis $ map yAxis moons
        zStates = iterate stepAxis $ map zAxis moons
        energies = map totalEnergy $ zipWith3 (zipWith3 zipMoon) xStates yStates zStates
    --mapM_ (mapM_ print >=> \_ -> putStrLn "") (take 11 xStates)
    return $ energies !! 1000

b :: IO Int
b = do
    moons <- readInput "input.txt"
    let xStates = iterate stepAxis $ map xAxis moons
        yStates = iterate stepAxis $ map yAxis moons
        zStates = iterate stepAxis $ map zAxis moons
        (xStart, xLoopLength) = loopStartAndLength xStates
        (yStart, yLoopLength) = loopStartAndLength yStates
        (zStart, zLoopLength) = loopStartAndLength zStates
    return $ maximum [xStart, yStart, zStart] + lcm xLoopLength (lcm yLoopLength zLoopLength)

main :: IO ()
main = do
    a >>= print
    b >>= print
