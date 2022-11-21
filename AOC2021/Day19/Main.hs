{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module AOC2021.Day19.Main where

import AOC2021.Common (Parser, justParse, int)
import Control.Applicative (Alternative(many, (<|>)), Applicative (liftA2))
import Text.Megaparsec (MonadParsec(eof), parseTest)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (eol)
import Control.Monad (void)
import Data.List (sort, permutations, find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe, fromJust)
import Control.Monad.State.Strict (evalState, MonadState (get, put), State)
import AOC2021.Day12.Main (justLookup)
import Debug.Trace (traceShow, trace, traceShowId)

type Point = [Int]
type ScannerId = Int
type Scanner = [Point]
type Dist = [Int]
type Rotation = Point -> Point
type Offset = [Int]
type Reorientation = Point -> Point
type ScannerReorientation = Scanner -> Scanner

point :: Parser Point
point = do
    x <- int
    ","
    y <- int
    ","
    z <- int
    return [x, y, z]

scanner :: Parser (ScannerId, Scanner)
scanner = do
    id <- "--- scanner " *> L.decimal <* " ---" <* eol
    ps <- many (point <* eol)
    return (id, ps)

scanners :: Parser [(ScannerId, Scanner)]
scanners =
    many (scanner <* (void eol <|> eof))


overlap :: Ord a => [a] -> [a] -> [a]
overlap xs ys = go sxs sys
  where
    sxs = sort xs
    sys = sort ys
    go [] _ = []
    go _ [] = []
    go (x : xs) (y : ys)
      | x == y = x : go xs ys
      | x > y  = go (x : xs) ys
      | x < y  = go xs (y : ys)

{-
uDist :: Point -> Point -> Dist
uDist [x1, y1, z1] [x2, y2, z2] = sort $ map abs [x1 - x2, y1 - y2, z1 - z2]

uDistPairs :: Scanner -> [[Dist]]
uDistPairs points = [[uDist p1 p2 | p2 <- points] | p1 <- points]

compatiblePoints :: [Dist] -> [Dist] -> Maybe [Dist]
compatiblePoints p1Dists p2Dists
  | length o >= 12 = Just o
  | otherwise = Nothing
  where
    o = overlap p1Dists p2Dists

scannerOverlap :: Scanner -> Scanner -> Maybe ()
scannerOverlap s1 s2 = undefined
  where
    udps1 = zip s1 (uDistPairs s1)
    udps2 = zip s2 (uDistPairs s2)
    lol = [[fmap (p1, p2, ) (compatiblePoints p1Dists p2Dists) | (p2, p2Dists) <- udps2] | (p1, p1Dists) <- udps1]
-}


--pointPermutations =
--    [ \[x, y, z] -> [x, y, z]
--    , \[x, y, z] -> [x, -z, y]
--    , \[x, y, z] -> [y, -x, z]
--    , \[x, y, z] -> [-y, -z, x]
--    , \[x, y, z] -> [-z, x, y]
--    , \[x, y, z] -> [-z, y, x]
--    ]
--pointPermutations =
--    [ \[x, y, z] -> [ x,  y,  z]
--
--    , \[x, y, z] -> [-y,  x,  z]
--    , \[x, y, z] -> [-x, -y,  z]
--    , \[x, y, z] -> [ y, -x,  z]
--
--    , \[x, y, z] -> [ z,  y, -x]
--    , \[x, y, z] -> [-x,  y, -z]
--    , \[x, y, z] -> [-z,  y,  x]
--
--    , \[x, y, z] -> [ x, -z,  y]
--    , \[x, y, z] -> [ x, -y, -z]
--    , \[x, y, z] -> [ x,  z, -y]
--    ]

pointNegations :: [Rotation]
pointNegations =
    [ \[x, y, z] -> [ x,  y,  z]
    , \[x, y, z] -> [ x, -y, -z]
    , \[x, y, z] -> [-x,  y, -z]
    , \[x, y, z] -> [-x, -y,  z]
    ]

rot90X :: Point -> Point
rot90X [x, y, z] = [ x, -z,  y]
rot90Y :: Point -> Point
rot90Y [x, y, z] = [ z,  y, -x]
rot90Z :: Point -> Point
rot90Z [x, y, z] = [-y,  x,  z]
--turn :: Point -> Point
--turn [x, y, z] = [-y,  x,  z]
--roll :: Point -> Point
--roll [x, y, z] = [ x,  z,  -y]

--rotations :: [Rotation]
--rotations = do
--    pp <- pointPermutations
--    pn <- pointNegations
--    return $ pn . pp
--rotations :: [Rotation]
--rotations =
--    [ id
--    , rot90X
--    , rot90Y
--    , rot90Z
--    , rot90X . rot90X
--    , rot90Y . rot90X
--    , rot90Z . rot90X
--    , rot90X . rot90Y
--    , rot90Y . rot90Y
--    , rot90Y . rot90Z
--    , rot90Z . rot90Z
--    , rot90X . rot90X . rot90X
--    , rot90Y . rot90X . rot90X
--    , rot90Z . rot90X . rot90X
--    , rot90X . rot90Y . rot90X
--    , rot90Y . rot90Y . rot90X
--    , rot90Z . rot90Z . rot90X
--    , rot90X . rot90X . rot90Y
--    , rot90Y . rot90Y . rot90Y
--    , rot90Z . rot90Z . rot90Z
--    , rot90Y . rot90X . rot90X . rot90X
--    , rot90X . rot90Y . rot90X . rot90X
--    , rot90X . rot90X . rot90Y . rot90X
--    , rot90Y . rot90Y . rot90Y . rot90X
--    ]
rotations :: [Rotation]
rotations =
    [ id
    , rot90X
    , rot90Y
    , rot90Z
    , rot90X . rot90X
    , rot90X . rot90Y
    , rot90X . rot90Z
    , rot90Y . rot90X
    , rot90Y . rot90Y
    , rot90Z . rot90Y
    , rot90Z . rot90Z
    , rot90X . rot90X . rot90X
    , rot90X . rot90X . rot90Y
    , rot90X . rot90X . rot90Z
    , rot90X . rot90Y . rot90X
    , rot90X . rot90Y . rot90Y
    , rot90X . rot90Z . rot90Z
    , rot90Y . rot90X . rot90X
    , rot90Y . rot90Y . rot90Y
    , rot90Z . rot90Z . rot90Z
    , rot90X . rot90X . rot90X . rot90Y
    , rot90X . rot90X . rot90Y . rot90X
    , rot90X . rot90Y . rot90X . rot90X
    , rot90X . rot90Y . rot90Y . rot90Y
    ]

reverseRotation :: Rotation -> Rotation
reverseRotation r = r . r . r

dist :: Point -> Point -> Dist
dist = zipWith (-)

distPairs :: Scanner -> [(Point, [Dist])]
distPairs points = [(p1, [dist p1 p2 | p2 <- points]) | p1 <- points]

scannerOverlap :: ScannerId -> ScannerId -> Scanner -> Scanner -> Maybe Offset
scannerOverlap s0id s1id s0 s1 =
    case foundOverlap of
        Nothing -> Nothing
        Just ((p1, _), (p2, _)) -> Just (dist p1 p2)
  where
    s0distPairs = distPairs s0
    s1distPairs = distPairs s1
    foundOverlap =
        find
            (\((p1, ds1), (p2, ds2)) ->
                let l = length (overlap ds1 ds2) in
                {-if s0id == 5 && l > 1 then
                    trace (show s0id ++ " <-> " ++ show s1id ++ " ==> " ++ show l) l >= 12
                else-}
                    length (overlap ds1 ds2) >= 12) $
                        liftA2 (,) s0distPairs s1distPairs

addOffset :: Offset -> Point -> Point
addOffset = zipWith (+)

subtractOffset :: Offset -> Point -> Point
subtractOffset = zipWith (-)

fst' :: (a, b, c) -> a
fst' (x, y, z) = x

snd' :: (a, b, c) -> b
snd' (x, y, z) = y

trd' :: (a, b, c) -> c
trd' (x, y, z) = z

findScannerOverlap :: ScannerId -> Scanner -> (ScannerId, Scanner) -> Maybe ((ScannerId, ScannerReorientation, Reorientation), [Point])
findScannerOverlap s0id s0 (s1id, s1) =
    case allOverlaps of
        [] -> {-trace ("No overlap found: " ++ show s0id) -}Nothing
        [ovrlp] -> {-trace ("Overlap found: " ++ show s0id ++ " and " ++ show (fst o)) $-} Just (ovrlp, overlappingPoints (snd' ovrlp))
        ovrlp : _ -> {-trace ("Overlap found: " ++ show s0id ++ " and " ++ show (fst o)) $-} Just (ovrlp, overlappingPoints (snd' ovrlp))
        --_ -> error "Found more than 1 rotation to overlap enough!"
  where
    allOverlaps = do
        rotation <- rotations
        let s1rotated = map rotation s1
        case scannerOverlap s0id s1id s0 s1rotated of
            Nothing -> []
            Just offset ->
                return (s1id, map (addOffset offset . rotation), addOffset offset . reverseRotation rotation)
    overlappingPoints reorient =
        overlap s0 (reorient s1)

otherScanners :: ScannerId -> Map ScannerId Scanner -> [(ScannerId, Scanner)]
otherScanners scannerId scanners = Map.toList $ Map.delete scannerId scanners

toScannerOverlapGraph :: Map ScannerId Scanner -> Map ScannerId (Scanner, [(ScannerId, ScannerReorientation, Reorientation)])
toScannerOverlapGraph scanners =
    Map.mapWithKey
        (\scannerId scanner ->
            (scanner, mapMaybe (fmap fst . (findScannerOverlap scannerId scanner)) (otherScanners scannerId scanners)))
        scanners

alignScanners :: Map ScannerId (Scanner, [(ScannerId, ScannerReorientation, Reorientation)]) -> Map ScannerId (Scanner, Point)
alignScanners scannerOverlapGraph =
    Map.fromList $ evalState (go 0 id id) Set.empty
  where
    go :: ScannerId -> ScannerReorientation -> Reorientation -> State (Set ScannerId) [(ScannerId, (Scanner, Point))]
    go scannerId reorient reverseReorient = do
        visited <- get
        if Set.member scannerId visited then
            return []
        else do
            put (Set.insert scannerId visited)
            let (scanner, neighbors) = justLookup scannerId scannerOverlapGraph
            rest <-
                concat <$>
                mapM (\(otherScannerId, innerReorient, innerReverseReorient) -> go otherScannerId (reorient . innerReorient) (innerReverseReorient . reverseReorient)) neighbors
            return $ (scannerId, (reorient scanner, {-reverseReorient [0, 0, 0]-} head (reorient [[0, 0, 0]]))) : rest

solve1 :: Map ScannerId Scanner -> Int
solve1 scanners = length allBeacons
  where
    scannerOverlapGraph = {-traceShow (map (\(k, (s, xs))-> (k, ({-s, -}map fst xs))) (Map.toList (toScannerOverlapGraph scanners))) $-} toScannerOverlapGraph scanners
    alignedScanners = alignScanners scannerOverlapGraph
    allBeacons = Set.unions $ map (Set.fromList . fst) $ Map.elems ({-trace (unlines $ map show $ Map.toList alignedScanners)-} alignedScanners)

manhattanDist :: Point -> Point -> Int
manhattanDist p1 p2 = sum $ map abs $ dist p1 p2

solve2 :: Map ScannerId Scanner -> Int
solve2 scanners = maximum $ map (uncurry manhattanDist) $ liftA2 (,) allScanners allScanners
  where
    scannerOverlapGraph = traceShow (map (\(k, (s, xs))-> (k, ({-s, -}map fst' xs))) (Map.toList (toScannerOverlapGraph scanners))) $ toScannerOverlapGraph scanners
    alignedScanners = alignScanners scannerOverlapGraph
    allScanners = map snd $ Map.elems (trace (unlines $ map (show . \(k, (v1, v2)) -> (k, v2)) $ Map.toList alignedScanners) alignedScanners)

main :: IO ()
main = do
    scanners <- Map.fromList . justParse scanners <$> readFile "AOC2021/Day19/input.txt"
    --scanners <- Map.fromList . justParse scanners <$> readFile "AOC2021/Day19/example.txt"
    --let [scanner0, scanner1, scanner2, scanner3, scanner4] = Map.elems scanners
    --print [scanner0, scanner1, scanner2, scanner3, scanner4]
    --mapM_ print $ snd $ fromJust $ findScannerOverlap 0 scanner0 (1, scanner1)
    print $ solve1 scanners
    print $ solve2 scanners
