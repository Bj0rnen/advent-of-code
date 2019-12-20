{-# LANGUAGE RecordWildCards #-}

import Data.Array.Unboxed
import Data.List
import Data.Maybe
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

findCoord :: Char -> UArray (Int, Int) Char -> (Int, Int)
findCoord c m = head [coord | coord <- range (bounds m), m ! coord == c]

countCoords :: (Char -> Bool) -> UArray (Int, Int) Char -> Int
countCoords pred m = length [coord | coord <- range (bounds m), pred (m ! coord)]

data State = State
    { numKeys :: Int
    , pos     :: (Int, Int)
    , keys    :: Set Char
    } deriving (Show, Eq, Ord)

data Cell = Invalid | Step | Key Char

visit :: Set Char -> Char -> Cell
visit ks cell
    | cell == '#' {-|| cell == '@'-} = Invalid
    | cell == '.' || cell == '@' = Step
    | cell >= 'A' && cell <= 'Z' =
        if Set.member (toLower cell) ks then Step else Invalid
    | cell >= 'a' && cell <= 'z' = Key cell

newState :: (Int, Int) -> Set Char -> Cell -> Maybe State
newState _ _ Invalid = Nothing
newState pos keys Step = Just (State { numKeys = Set.size keys, ..})
newState pos oldKeys (Key k) =
    let keys = Set.insert k oldKeys
    in  Just (State { numKeys = Set.size keys, ..})

nextStates :: UArray (Int, Int) Char -> State -> Set State
nextStates m (State n (x, y) ks) =
    let dirs =
            [ (x    , y - 1)
            , (x    , y + 1)
            , (x - 1, y    )
            , (x + 1, y    )
            ]
    in  Set.fromList $ catMaybes $ map (\pos -> newState pos ks (visit ks (m ! pos))) dirs

solve1 :: UArray (Int, Int) Char -> Int
solve1 m = go m 0 (Set.singleton (State 0 start Set.empty)) Set.empty
    where
        start = findCoord '@' m
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


a :: IO Int
a = do
    ls <- lines <$> readFile "input.txt"
    let rows = length ls
        cols = length $ ls !! 0
        m = listArray ((0, 0), (cols - 1, rows - 1)) (concat $ transpose ls)
    return $ solve1 m

b :: IO Int
b = do
    undefined

main :: IO ()
main = do
    a >>= print
    --b >>= print
