{-# LANGUAGE RecordWildCards #-}

import Data.Array.Unboxed
import Data.List
import Data.Maybe
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

findCoords :: Char -> UArray (Int, Int) Char -> [(Int, Int)]
findCoords c m = [coord | coord <- range (bounds m), m ! coord == c]

findCoord :: Char -> UArray (Int, Int) Char -> (Int, Int)
findCoord c = head . findCoords c

countCoords :: (Char -> Bool) -> UArray (Int, Int) Char -> Int
countCoords pred m = length [coord | coord <- range (bounds m), pred (m ! coord)]

data State = State
    { numKeys :: Int
    , pos     :: [(Int, Int)]
    , keys    :: Set Char
    } deriving (Show, Eq, Ord)

data Cell = Invalid | Step | Key Char
    deriving (Show)

visit :: Set Char -> Char -> Cell
visit ks cell
    | cell == '#' {-|| cell == '@'-} = Invalid
    | cell == '.' || cell == '@' = Step
    | cell >= 'A' && cell <= 'Z' =
        if Set.member (toLower cell) ks then Step else Invalid
    | cell >= 'a' && cell <= 'z' = Key cell

isInteresting :: Char -> Bool
isInteresting cell
    | cell == '@' = True
    | cell >= 'a' && cell <= 'z' = True
    | otherwise = False

multipleNonInteresting :: [Char] -> Bool
multipleNonInteresting = (> 1) . length . filter (not . isInteresting)

newState :: [(Int, Int)] -> [Char] -> Set Char -> Cell -> Maybe State
newState _ _ _ Invalid = Nothing
newState pos cs keys Step =
    if multipleNonInteresting cs then
        Nothing
    else
        Just (State { numKeys = Set.size keys, ..})
newState pos cs oldKeys (Key k) =
    if multipleNonInteresting cs then
        Nothing
    else
        let keys = Set.insert k oldKeys
        in  Just (State { numKeys = Set.size keys, ..})

onEachIndividually :: (a -> (a, b)) -> [a] -> [([a], b)]
onEachIndividually f [] = []
onEachIndividually f (x : xs) =
    (let (x', y) = f x in (x' : xs, y))
    :
    (map (\(xs', y) -> (x : xs', y)) (onEachIndividually f xs))

nextStates :: UArray (Int, Int) Char -> State -> Set State
nextStates m (State n ps ks) =
    let alternatives = do
            f <-
                [ \(x, y) -> (x    , y - 1)
                , \(x, y) -> (x    , y + 1)
                , \(x, y) -> (x - 1, y    )
                , \(x, y) -> (x + 1, y    )
                ]
            onEachIndividually (fmap (\coord -> (coord, visit ks (m ! coord))) f) ps
    in  Set.fromList $ catMaybes $ map (\(ps', cell) -> newState ps' (map (\p -> m ! p) ps') ks cell) alternatives

solve :: UArray (Int, Int) Char -> Int
solve m = go m 0 (Set.singleton (State 0 entrances Set.empty)) Set.empty
    where
        entrances = findCoords '@' m
        totalKeys = countCoords (\c -> c >= 'a' && c <= 'z') m
        go m steps states statesEverTried
            | (trace "Max keys found:" (traceShowId (numKeys (Set.findMax states)))) == totalKeys = steps
            | otherwise =
                trace ("Steps taken: " ++ show steps) $
                    go
                        m
                        (steps + 1)
                        (Set.difference (Set.foldl' (\ss s -> Set.union ss (nextStates m s)) Set.empty states) statesEverTried)
                        (Set.union statesEverTried states)

patch :: UArray (Int, Int) Char -> UArray (Int, Int) Char
patch m =
    let (x, y) = findCoord '@' m
    in  m // [ ((x - 1, y - 1), '@'), ((x    , y - 1), '#'), ((x + 1, y - 1), '@')
             , ((x - 1, y    ), '#'), ((x    , y    ), '#'), ((x + 1, y    ), '#')
             , ((x - 1, y + 1), '@'), ((x    , y + 1), '#'), ((x + 1, y + 1), '@')
             ]

parse :: String -> UArray (Int, Int) Char
parse s =
    let ls = lines s
        rows = length ls
        cols = length $ ls !! 0
    in  listArray ((0, 0), (cols - 1, rows - 1)) (concat $ transpose ls)

a :: IO Int
a = do
    m <- parse <$> readFile "input.txt"
    return $ solve m

b :: IO Int
b = do
    m <- patch . parse <$> readFile "input.txt"
    return $ solve m

main :: IO ()
main = do
    --a >>= print
    b >>= print
