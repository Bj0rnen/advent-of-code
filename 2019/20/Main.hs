{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Data.Array.Unboxed
import Data.List
import Data.Maybe
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad

import Debug.Trace

data Innerness = Inner | Outer
    deriving (Show, Eq, Ord)

data Cell = Wall | Open | Portal String Innerness [(Int -> Int, (Int, Int))]

data State = State { level :: Int, position :: (Int, Int) }
    deriving (Show, Eq, Ord)

isWall :: Cell -> Bool
isWall Wall = True
isWall _ = False

isEffectivelyWall :: Int -> Cell -> Bool
isEffectivelyWall _ Wall = True
isEffectivelyWall 0 (Portal "AA" Outer _) = False
isEffectivelyWall 0 (Portal "ZZ" Outer _) = False
isEffectivelyWall 0 (Portal _ Outer _) = True
isEffectivelyWall _ (Portal "AA" Outer _) = True
isEffectivelyWall _ (Portal "ZZ" Outer _) = True
isEffectivelyWall _ _ = False

isPortal :: Cell -> Bool
isPortal (Portal _ _ _) = True
isPortal _ = False

isZZ :: Cell -> Bool
isZZ (Portal "ZZ" _ _) = True
isZZ _ = False

nextStates1 :: Array (Int, Int) Cell -> (Int, Int) -> Set (Int, Int)
nextStates1 donut (x, y) =
    let alternatives =
            case donut ! (x, y) of
                Open ->
                    filter (not . isWall . (donut !))
                    [ (x    , y - 1)
                    , (x    , y + 1)
                    , (x - 1, y    )
                    , (x + 1, y    )
                    ]
                Portal _ _ alts -> map snd alts
    in  Set.fromList alternatives

nextStates2 :: Array (Int, Int) Cell -> State -> Set State
nextStates2 donut (State level (x, y)) =
    let alternatives =
            case donut ! (x, y) of
                Open ->
                    map (State level) $ filter (not . isEffectivelyWall level . (donut !))
                    [ (x    , y - 1)
                    , (x    , y + 1)
                    , (x - 1, y    )
                    , (x + 1, y    )
                    ]
                Portal _ _ alts -> map (\(modLevel, p) -> State (modLevel level) p) alts
    in  Set.fromList alternatives

solve1 :: Array (Int, Int) Cell -> (Int, Int) -> Int
solve1 donut aa =
    go 0 [aa] Set.empty
    where
        go :: Int -> [(Int, Int)] -> Set (Int, Int) -> Int
        go steps states statesEverTried =
            case find isZZ $ map (donut !) states of
                Just (Portal "ZZ" _ _) -> steps
                Nothing ->
                    go
                        (steps + 1)
                        (Set.toList (Set.difference (foldl' (\ss s -> Set.union ss (nextStates1 donut s)) Set.empty states) statesEverTried))
                        (Set.union statesEverTried (Set.fromList states))

solve2 :: Array (Int, Int) Cell -> (Int, Int) -> Int
solve2 donut aa =
    go 0 [State 0 aa] Set.empty
    where
        go :: Int -> [State] -> Set State -> Int
        go steps states statesEverTried =
            case find isZZ $ map (\s -> donut ! position s) states of
                Just (Portal "ZZ" _ _) -> steps
                Nothing ->
                    go
                        (steps + 1)
                        (Set.toList (Set.difference (foldl' (\ss s -> Set.union ss (nextStates2 donut s)) Set.empty states) statesEverTried))
                        (Set.union statesEverTried (Set.fromList states))

parse :: String -> (Array (Int, Int) Cell, (Int, Int))
parse s = (donut, aa)
    where
        ls = lines s
        rows = length ls
        cols = length $ ls !! 0
        whole = listArray ((0, 0), (cols - 1, rows - 1)) (concat $ transpose ls) :: UArray (Int, Int) Char
        donutPlusHole = array ((2, 2), (cols - 3, rows - 3)) (map (\p -> (p, whole ! p)) (range ((2, 2), (cols - 3, rows - 3)))) :: UArray (Int, Int) Char
        emptyPositionsInHole = map fst $ filter (\(p, c) -> c == ' ') $ assocs donutPlusHole
        (holeMinX, holeMinY) = head emptyPositionsInHole
        (holeMaxX, holeMaxY) = last emptyPositionsInHole
        donutPlusHoleCoords = Set.fromList $ indices donutPlusHole
        holeCoords = Set.fromList $ range ((holeMinX, holeMinY), (holeMaxX, holeMaxY))
        donutCoords = Set.toList $ Set.difference donutPlusHoleCoords holeCoords
        toCell (x, y)
            | whole ! (x, y) == '#' = Wall
            | x ==            2                                   =
                let label = [whole ! (           0,            y), whole ! (           1,            y)]
                in  Portal label Outer ((id, (x + 1, y    )) : (map ((subtract 1),) $ filter (/= (x, y)) $ portals Map.! label))
            | x == holeMinX - 1 && y >= holeMinY && y <= holeMaxY =
                let label = [whole ! (holeMinX    ,            y), whole ! (holeMinX + 1,            y)]
                in  Portal label Inner ((id, (x - 1, y    )) : (map ((       + 1),) $ filter (/= (x, y)) $ portals Map.! label))
            | x == holeMaxX + 1 && y >= holeMinY && y <= holeMaxY =
                let label = [whole ! (holeMaxX - 1,            y), whole ! (holeMaxX    ,            y)]
                in  Portal label Inner ((id, (x + 1, y    )) : (map ((       + 1),) $ filter (/= (x, y)) $ portals Map.! label))
            | x ==     cols - 3                                   =
                let label = [whole ! (    cols - 2,            y), whole ! (    cols - 1,            y)]
                in  Portal label Outer ((id, (x - 1, y    )) : (map ((subtract 1),) $ filter (/= (x, y)) $ portals Map.! label))
            | y ==            2                                   =
                let label = [whole ! (           x,            0), whole ! (           x,            1)]
                in  Portal label Outer ((id, (x    , y + 1)) : (map ((subtract 1),) $ filter (/= (x, y)) $ portals Map.! label))
            | y == holeMinY - 1 && x >= holeMinX && x <= holeMaxX =
                let label = [whole ! (           x, holeMinY    ), whole ! (           x, holeMinY + 1)]
                in  Portal label Inner ((id, (x    , y - 1)) : (map ((       + 1),) $ filter (/= (x, y)) $ portals Map.! label))
            | y == holeMaxY + 1 && x >= holeMinX && x <= holeMaxX =
                let label = [whole ! (           x, holeMaxY - 1), whole ! (           x, holeMaxY    )]
                in  Portal label Inner ((id, (x    , y + 1)) : (map ((       + 1),) $ filter (/= (x, y)) $ portals Map.! label))
            | y ==     rows - 3                                   =
                let label = [whole ! (           x,     rows - 2), whole ! (           x,     rows - 1)]
                in  Portal label Outer ((id, (x    , y - 1)) : (map ((subtract 1),) $ filter (/= (x, y)) $ portals Map.! label))
            | otherwise = Open
        portals :: Map String [(Int, Int)]
        portals = Map.fromListWith (++) $ map (\(coord, (Portal label _ _)) -> (label, [coord])) $ filter (\(_, cell) -> isPortal cell) $ map (\p -> (p, donut ! p)) donutCoords
        donut = array ((0, 0), (cols - 1, rows - 1)) [(p, toCell p) | p <- donutCoords]
        aa = head $ portals Map.! "AA"

plot :: UArray (Int, Int) Char -> IO ()
plot m =
    forM_ [snd (fst (bounds m))..snd (snd (bounds m))] $ \y -> do
        forM_ [fst (fst (bounds m))..fst (snd (bounds m))] $ \x ->
            putChar (m ! (x, y))
        putStrLn ""

a :: IO Int
a = do
    (donut, aa) <- parse <$> readFile "input.txt"
    return $ solve1 donut aa

b :: IO Int
b = do
    (donut, aa) <- parse <$> readFile "input.txt"
    return $ solve2 donut aa

main :: IO ()
main = do
    a >>= print
    b >>= print
