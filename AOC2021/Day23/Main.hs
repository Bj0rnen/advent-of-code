{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module AOC2021.Day23.Main where

import Data.Array.Unboxed (UArray, listArray, assocs, (!), IArray, Array, Ix, (//), array, elems)
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShowId)

-- This wrong answer was too high.
upperBound :: Int
upperBound = 48067

-- "completed" is how many amphipods are at their destination.
-- Last element in "stack" is "completed" steps above the bottom of the room.
-- Amphipods at their destination are not in "stack".
data Room = MkRoom
    { stack     :: [Char]
    , completed :: Int
    }
    deriving stock (Show, Eq, Ord)

data GameState = MkGameState
    { hallway :: !(UArray Int Char)  -- (0, 6), rooms right side of 1, 2, 3 & 4
    , rooms   :: !(Array Int Room)  -- (1, 4)
    } deriving (Show, Eq, Ord)

startState :: GameState
startState = MkGameState
    { hallway = listArray (0, 6) ['.', '.', '.', '.', '.', '.', '.']
    , rooms = listArray (1, 4)
        [ MkRoom "BDDD" 0
        , MkRoom "ACBC" 0
        , MkRoom "ABAB" 0
        , MkRoom "DACC" 0
        ]
    }

data AmphipodAddress = Hallway Int | Room Int

movableAmphipods :: GameState -> [AmphipodAddress]
movableAmphipods (MkGameState hallway rooms) =
    hw ++ rs
  where
    hw = map (Hallway . fst) $ filter ((/= '.') . snd) $ assocs hallway
    rs = map (Room . fst) $ filter (not . null . stack . snd) $ assocs rooms

amphipodType :: GameState -> AmphipodAddress -> Char
amphipodType state (Hallway i) = hallway state ! i
amphipodType state (Room i) = head (stack (rooms state ! i))

typeToRoomIndex :: Char -> Int
typeToRoomIndex 'A' = 1
typeToRoomIndex 'B' = 2
typeToRoomIndex 'C' = 3
typeToRoomIndex 'D' = 4

typeToCost :: Char -> Int
typeToCost 'A' = 1
typeToCost 'B' = 10
typeToCost 'C' = 100
typeToCost 'D' = 1000

setArray :: (IArray a e, Ix i) => a i e -> i -> e -> a i e
setArray arr i x = arr // [(i, x)]

actualHallwayPosition :: Int -> Int
actualHallwayPosition = \case
    0 -> 0
    1 -> 1
    2 -> 3
    3 -> 5
    4 -> 7
    5 -> 9
    6 -> 10

hallwayIndex :: Int -> Maybe Int
hallwayIndex = \case
    0  -> Just 0
    1  -> Just 1
    3  -> Just 2
    5  -> Just 3
    7  -> Just 4
    9  -> Just 5
    10 -> Just 6
    _  -> Nothing

hallwayDistanceActual :: Int -> Int -> Int
hallwayDistanceActual actualFrom actualTo = abs (actualFrom - actualTo)

actualRoomPosition :: Int -> Int
actualRoomPosition = \case
    1 -> 2
    2 -> 4
    3 -> 6
    4 -> 8

undirectedRange :: (Enum a, Ord a) => a -> a -> [a]
undirectedRange x y
  | x <= y    = [x..y]
  | otherwise = [y..x]

hallwayUnblockedIncludingFrom :: UArray Int Char -> Int -> Int -> Bool
hallwayUnblockedIncludingFrom hallway actualFrom actualTo =
    all (== '.') tiles
  where
    actualPositions = undirectedRange actualFrom actualTo
    indices = mapMaybe hallwayIndex actualPositions
    tiles = map (hallway !) indices

hallwayUnblockedExcludingFrom :: UArray Int Char -> Int -> Int -> Bool
hallwayUnblockedExcludingFrom hallway actualFrom actualTo
  | actualFrom == actualTo = True
  | actualFrom > actualTo  = hallwayUnblockedIncludingFrom hallway (actualFrom - 1) actualTo
  | otherwise              = hallwayUnblockedIncludingFrom hallway (actualFrom + 1) actualTo

stepsOutOfRoom :: Room -> Int
stepsOutOfRoom room = 5 - (length (stack room) + completed room)

tryMoveToRoom :: GameState -> AmphipodAddress -> Maybe (GameState, Int)
tryMoveToRoom state address =
    if not (null (stack destRoom)) then
        Nothing
    else
        case address of
            Hallway i ->
                if unblocked then
                    Just (state', energy)
                else
                    Nothing
              where
                actualHallwayPos = actualHallwayPosition i
                unblocked = hallwayUnblockedExcludingFrom (hallway state) actualHallwayPos actualDestRoomPos
                hallway' = setArray (hallway state) i '.'
                state' = MkGameState hallway' rooms'
                stepsUp = 0
                stepsInHallway = hallwayDistanceActual actualHallwayPos actualDestRoomPos
                energy = typeToCost t * (stepsInHallway + stepsDown)
            Room fromRoomNumber ->
                if unblocked then
                    Just (state', energy)
                else
                    Nothing
              where
                fromRoom = rooms state ! fromRoomNumber
                actualFromRoomPos = actualRoomPosition fromRoomNumber
                unblocked = hallwayUnblockedIncludingFrom (hallway state) actualFromRoomPos actualDestRoomPos
                hallway' = hallway state
                fromRoom' = MkRoom (tail (stack fromRoom)) (completed fromRoom)
                rooms'' = setArray rooms' fromRoomNumber fromRoom'
                state' = MkGameState hallway' rooms''
                stepsUp = stepsOutOfRoom fromRoom
                stepsInHallway = hallwayDistanceActual actualFromRoomPos actualDestRoomPos
                energy = typeToCost t * (stepsUp + stepsInHallway + stepsDown)
  where
    t = amphipodType state address
    destRoomNumber = typeToRoomIndex t
    actualDestRoomPos = actualRoomPosition destRoomNumber
    destRoom = rooms state ! destRoomNumber
    stepsDown = 4 - completed destRoom
    destRoom' =
        MkRoom
            (if stack destRoom /= [] then error "stack not empty" else [])
            (if completed destRoom + 1 > 4 then error "completed > 4" else completed destRoom + 1)
    rooms' = setArray (rooms state) destRoomNumber destRoom'

tryMoveToHallway :: GameState -> Int -> Int -> Maybe (GameState, Int)
tryMoveToHallway state roomNumber hallwayPosition =
    if unblocked then
        Just (state', energy)
    else
        Nothing
  where
    actualRoomPos = actualRoomPosition roomNumber
    actualHallwayPos = actualHallwayPosition hallwayPosition
    unblocked = hallwayUnblockedIncludingFrom (hallway state) actualRoomPos actualHallwayPos
    room = rooms state ! roomNumber
    t = amphipodType state (Room roomNumber)
    hallway' = setArray (hallway state) hallwayPosition t
    room' = MkRoom (tail (stack room)) (completed room)
    rooms' = setArray (rooms state) roomNumber room'
    state' = MkGameState hallway' rooms'
    stepsUp = stepsOutOfRoom room
    stepsInHallway = hallwayDistanceActual actualRoomPos actualHallwayPos
    energy = typeToCost t * (stepsUp + stepsInHallway)


possibleRoomToHallwayMoves :: GameState -> Int -> [(GameState, Int)]
possibleRoomToHallwayMoves state roomNumber = mapMaybe (tryMoveToHallway state roomNumber) [0..6]

possibleMoves :: GameState -> [(GameState, Int)]
possibleMoves state =
    if not (null toRooms) then
        [head toRooms]
    else
        concatMap (possibleRoomToHallwayMoves state) inRoomMovables
  where
    movables = movableAmphipods state
    toRooms = mapMaybe (tryMoveToRoom state) movables
    inRoomMovables = concatMap (\case Hallway _ -> []; Room i -> [i]) movables

--paths :: GameState -> [Int]
--paths state
--  | all ((== 4) . completed) $ rooms state = [0]
--  | otherwise = do
--    (state', cost) <- possibleMoves state
--    pathCost <- paths state'
--    return $ cost + pathCost

paths :: Int -> GameState -> [Int]
paths accCost state
--  | accCost >= upperBound = []  -- This does verly little to improve running time.
  | all ((== 4) . completed) $ rooms state = [accCost]
  | otherwise = do
    (state', cost) <- possibleMoves state
    paths (accCost + cost) state'

main :: IO ()
main = do
    print $ minimum (paths 0 startState)
