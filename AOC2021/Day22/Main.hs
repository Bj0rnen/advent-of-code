{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module AOC2021.Day22.Main where

import AOC2021.Common (justParse, Parser, int)
import Control.Applicative (Alternative((<|>)))
import Text.Megaparsec.Char (char)
import Data.Array.Unboxed (elems, array, Ix (range, inRange, rangeSize), UArray)
import Data.List (foldl', find, (\\))
import Data.Maybe (mapMaybe, isJust)
import Debug.Trace (trace, traceShow)

axis :: Char -> Parser (Int, Int)
axis c = do
    char c
    "="
    lo <- int
    ".."
    hi <- int
    return (lo, hi)

rebootStep :: Parser (Bool, ((Int, Int, Int), (Int, Int, Int)))
rebootStep = do
    on <- True <$ "on" <|> False <$ "off"
    " "
    (xLo, xHi) <- axis 'x'
    ","
    (yLo, yHi) <- axis 'y'
    ","
    (zLo, zHi) <- axis 'z'
    return (on, ((xLo, yLo, zLo), (xHi, yHi, zHi)))

bnds :: ((Int, Int, Int), (Int, Int, Int))
bnds = ((-50, -50, -50), (50, 50, 50))

inRegion :: (Bool, ((Int, Int, Int), (Int, Int, Int))) -> Bool
inRegion (_, ((xLo, yLo, zLo), (xHi, yHi, zHi))) =
    xLo >= -50 && yLo >= -50 && zLo >= -50 &&
    xHi <=  50 && yHi <=  50 && zHi <=  50

onAfterLastStep :: [(Bool, ((Int, Int, Int), (Int, Int, Int)))] -> (Int, Int, Int) -> Bool -> Bool
onAfterLastStep [] coord current = current
onAfterLastStep ((on, range) : steps) coord current =
    onAfterLastStep steps coord (if inRange range coord then on else current)

solve1 :: [(Bool, ((Int, Int, Int), (Int, Int, Int)))] -> Int
solve1 rebootSteps = countCubes cuboidMap
  where
    cuboidMap = foldl' applyStep [] (filter inRegion rebootSteps)
    applyStep cuboidMap (False, cuboid) = trace ("off " ++ show cuboid) $ removeCuboidFromMap cuboid cuboidMap
    applyStep cuboidMap (True, cuboid) = trace ("on " ++ show cuboid) $ addCuboidToMap cuboid cuboidMap
--solve1 rebootSteps = length $ filter id $ elems finalCube
--  where
--    inRegionSteps = filter inRegion rebootSteps
--    finalCube =
--        array bnds [(coord, onAfterLastStep inRegionSteps coord False) | coord <- range bnds] :: UArray (Int, Int, Int) Bool


type Cuboid = ((Int, Int, Int), (Int, Int, Int))

nonEmptyCuboid :: Cuboid -> Bool
nonEmptyCuboid cuboid = not (null (range cuboid))

overlap :: Cuboid -> Cuboid -> Maybe Cuboid
overlap ((x1Lo, y1Lo, z1Lo), (x1Hi, y1Hi, z1Hi)) ((x2Lo, y2Lo, z2Lo), (x2Hi, y2Hi, z2Hi)) =
    if nonEmptyCuboid candidate then
        Just candidate
    else
        Nothing
  where
    xLo = max x1Lo x2Lo
    yLo = max y1Lo y2Lo
    zLo = max z1Lo z2Lo
    xHi = min x1Hi x2Hi
    yHi = min y1Hi y2Hi
    zHi = min z1Hi z2Hi
    candidate = ((xLo, yLo, zLo), (xHi, yHi, zHi))

-- Invariant: overlap cuboid hole == hole
subtractCuboid :: Cuboid -> Cuboid -> [Cuboid]
subtractCuboid
  ((xHoleLo, yHoleLo, zHoleLo), (xHoleHi, yHoleHi, zHoleHi))
  ((xCuboidLo, yCuboidLo, zCuboidLo), (xCuboidHi, yCuboidHi, zCuboidHi)) =
    filter nonEmptyCuboid [left, right, front, back, top, bottom]
  where
    left   = ((xCuboidLo  , yCuboidLo  , zCuboidLo  ), (xHoleLo - 1, yCuboidHi  , zCuboidHi  ))
    right  = ((xHoleHi + 1, yCuboidLo  , zCuboidLo  ), (xCuboidHi  , yCuboidHi  , zCuboidHi  ))
    front  = ((xHoleLo    , yCuboidLo  , zCuboidLo  ), (xHoleHi    , yCuboidHi  , zHoleLo - 1))
    back   = ((xHoleLo    , yCuboidLo  , zHoleHi + 1), (xHoleHi    , yCuboidHi  , zCuboidHi  ))
    bottom = ((xHoleLo    , yCuboidLo  , zHoleLo    ), (xHoleHi    , yHoleLo - 1, zHoleHi    ))
    top    = ((xHoleLo    , yHoleHi + 1, zHoleLo    ), (xHoleHi    , yCuboidHi  , zHoleHi    ))

safeSubtractCuboid :: Cuboid -> Cuboid -> [Cuboid]
safeSubtractCuboid hole cuboid =
    case overlap cuboid hole of
        Nothing -> [cuboid]
        Just hole' -> subtractCuboid hole' cuboid

subtractCuboids :: [Cuboid] -> Cuboid -> [Cuboid]
subtractCuboids holes cuboid = go holes [cuboid]
  where
    go [] subCuboids = subCuboids
    go (hole : holes) subCuboids =
        go holes $ concatMap (safeSubtractCuboid hole) subCuboids

type CuboidMap = [Cuboid]

--addCuboidToMap :: Cuboid -> CuboidMap -> CuboidMap
--addCuboidToMap cuboid cuboidMap = finalCuboidsToAdd ++ cuboidMap
--  where
--    overlaps = mapMaybe (overlap cuboid) cuboidMap
--    finalCuboidsToAdd = subtractCuboids overlaps cuboid

addCuboidToMap :: Cuboid -> CuboidMap -> CuboidMap
addCuboidToMap cuboid cuboidMap = finalCuboidsToAdd ++ cuboidMap'
  where
    overlapsAndOriginals = mapMaybe (\x -> fmap (, x) (overlap cuboid x)) cuboidMap
    totalOverlaps = map snd $ filter (uncurry (==)) overlapsAndOriginals
    cuboidMap' = cuboidMap \\ totalOverlaps
    overlaps = map fst overlapsAndOriginals \\ totalOverlaps
    finalCuboidsToAdd = subtractCuboids overlaps cuboid

removeCuboidFromMap :: Cuboid -> CuboidMap -> CuboidMap
removeCuboidFromMap hole = concatMap (safeSubtractCuboid hole)

countCubes :: CuboidMap -> Int
countCubes cuboidMap = sum $ map rangeSize cuboidMap

solve2 :: [(Bool, ((Int, Int, Int), (Int, Int, Int)))] -> Int
solve2 rebootSteps = countCubes cuboidMap
  where
    cuboidMap = foldl' applyStep [] $ zip [1..] rebootSteps
    applyStep cuboidMap (step, (False, cuboid)) = trace ("step " ++ show step ++ ", length cuboidMap = " ++ show (length cuboidMap)) $ removeCuboidFromMap cuboid cuboidMap
    applyStep cuboidMap (step, (True, cuboid)) = trace ("step " ++ show step ++ ", length cuboidMap = " ++ show (length cuboidMap)) $ addCuboidToMap cuboid cuboidMap

main :: IO ()
main = do
    rebootSteps <- map (justParse rebootStep) . lines <$> readFile "AOC2021/Day22/input.txt"
    print $ solve1 rebootSteps
    print $ solve2 rebootSteps
