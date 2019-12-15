{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}

import Text.Megaparsec hiding (getInput)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor
import Data.Void
import Data.Maybe
import Data.Array.MArray
import Data.Array.ST
import Control.Monad
import Control.Monad.ST.Lazy
import Control.Monad.RWS
import Control.Monad.Trans.Class
import Data.Bifunctor
import Data.List
import Data.Ord
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

type Parser = Parsec Void String

justParse :: (Stream s, Ord e) => Parsec e s a -> s -> a
justParse p s = fromJust $ parseMaybe p s

integers :: Parser [Int]
integers = do
    n <- L.signed (return ()) L.decimal
    ns <-   char ',' *> integers
        <|> (eof <|> void newline) $> []
    return (n : ns)

readInput :: IO [Int]
readInput = do
    s <- readFile "input.txt"
    return $ justParse integers s

initialize :: MArray arr Int m => Int -> [Int] -> m (arr Int Int)
initialize memorySize input =
    newListArray (0, memorySize - 1) (input ++ [0, 0..])

data ICState = ICState
    { stdin :: [Int]
    --, pc    :: Int
    , rb   :: Int
    }

newtype Intcode arr m a =
    Intcode { runIntcode :: RWST (arr Int Int) [Int] ICState m a }
    deriving newtype (Functor, Applicative, Monad, MonadReader (arr Int Int), MonadWriter [Int], MonadState ICState, MonadTrans)

evalIntcode :: MArray arr Int m => arr Int Int -> ICState -> Intcode arr m a -> m (a, [Int])
evalIntcode arr inp s = evalRWST (runIntcode s) arr inp

readMem :: MArray arr Int m => Int -> Intcode arr m Int
readMem i = do
    arr <- ask
    lift $ readArray arr i

writeMem :: MArray arr Int m => Int -> Int -> Intcode arr m ()
writeMem i x = do
    arr <- ask
    lift $ writeArray arr i x

fetch :: MArray arr Int m => ParamMode -> Int -> Intcode arr m Int
fetch Position i = readMem i >>= readMem
fetch Immediate i = readMem i
fetch Relative i = ((+) <$> readMem i <*> gets rb) >>= readMem

write :: MArray arr Int m => ParamMode -> Int -> Int -> Intcode arr m ()
write Position i val = readMem i >>= \ri -> writeMem ri val
write Immediate i val = error "Immediate is not a supported parameter mode when writing to memory"
write Relative i val = ((+) <$> readMem i <*> gets rb) >>= \ri -> writeMem ri val

getInput :: MArray arr Int m => Intcode arr m Int
getInput = do
    x <- gets (head . stdin)
    modify (\ICState {..} -> ICState { stdin = tail stdin, .. })
    return x

getOutput :: MArray arr Int m => Intcode arr m Int
getOutput = readMem 0

binOp :: MArray arr Int m => (Int -> Int -> Int) -> Int -> ParamMode -> ParamMode -> ParamMode -> Intcode arr m ()
binOp op i m1 m2 m3 = do
    x <- fetch m1 (i + 1)
    y <- fetch m2 (i + 2)
    write m3 (i + 3) (x `op` y)

boolOp :: MArray arr Int m => (Int -> Int -> Bool) -> Int -> ParamMode -> ParamMode -> ParamMode -> Intcode arr m ()
boolOp op i m1 m2 m3 = do
    x <- fetch m1 (i + 1)
    y <- fetch m2 (i + 2)
    write m3 (i + 3) (if x `op` y then 1 else 0)

input :: MArray arr Int m => Int -> ParamMode -> Intcode arr m ()
input i m1 = do
    x <- getInput
    write m1 (i + 1) x

output :: MArray arr Int m => Int -> ParamMode -> Intcode arr m ()
output i m1 = do
    x <- fetch m1 (i + 1)
    tell [x]

jumpIf :: MArray arr Int m => Bool -> Int -> ParamMode -> ParamMode -> Intcode arr m (Maybe Int)
jumpIf b i m1 m2 = do
    x <- fetch m1 (i + 1)
    if (x /= 0 && b) || (x == 0 && not b) then
        Just <$> fetch m2 (i + 2)
    else
        return Nothing

adjustRelativeBase :: MArray arr Int m => Int -> ParamMode -> Intcode arr m ()
adjustRelativeBase i m1 = do
    x <- fetch m1 (i + 1)
    modify (\ICState {..} -> ICState { rb = rb + x, ..})

data ParamMode = Position | Immediate | Relative
    deriving (Show, Enum)

-- Param is 1-indexed
paramMode :: Int -> Int -> ParamMode
paramMode param instr = toEnum ((instr `div` (10 ^ (param+1))) `mod` 10)

run :: MArray arr Int m => Intcode arr m Int
run = go 0
    where
        go i = do
            instruction <- readMem i
            let opCode = instruction `mod` 100
            case opCode of
                1 -> do
                    binOp (+) i (paramMode 1 instruction) (paramMode 2 instruction) (paramMode 3 instruction)
                    go (i + 4)
                2 -> do
                    binOp (*) i (paramMode 1 instruction) (paramMode 2 instruction) (paramMode 3 instruction)
                    go (i + 4)
                3 -> do
                    input i (paramMode 1 instruction)
                    go (i + 2)
                4 -> do
                    output i (paramMode 1 instruction)
                    go (i + 2)
                5 -> do
                    jump <- jumpIf True i (paramMode 1 instruction) (paramMode 2 instruction)
                    case jump of
                        Nothing -> go (i + 3)
                        Just to -> go to
                6 -> do
                    jump <- jumpIf False i (paramMode 1 instruction) (paramMode 2 instruction)
                    case jump of
                        Nothing -> go (i + 3)
                        Just to -> go to
                7 -> do
                    boolOp (<) i (paramMode 1 instruction) (paramMode 2 instruction) (paramMode 3 instruction)
                    go (i + 4)
                8 -> do
                    boolOp (==) i (paramMode 1 instruction) (paramMode 2 instruction) (paramMode 3 instruction)
                    go (i + 4)
                9 -> do
                    adjustRelativeBase i (paramMode 1 instruction)
                    go (i + 2)
                99 ->
                    getOutput

runProgram :: MArray arr Int m => arr Int Int -> ICState -> m [Int]
runProgram memory icState = snd <$> evalIntcode memory icState run


rev :: Int -> Int
rev 1 = 2
rev 2 = 1
rev 3 = 4
rev 4 = 3

move :: Int -> (Int, Int) -> (Int, Int)
move 1 (x, y) = (x    , y - 1)
move 2 (x, y) = (x    , y + 1)
move 3 (x, y) = (x - 1, y    )
move 4 (x, y) = (x + 1, y    )

stepThen :: Int -> [Int] -> ([Int] -> [Maybe Int]) -> [Maybe Int]
stepThen dir ~(status1 : rest) f =
    Just dir :
        case status1 of
            0 -> []
            1 -> f rest ++ [Just (rev dir)]
            2 -> Nothing : f rest ++ [Just (rev dir)]
iddfs :: (Int, Int) -> Set (Int, Int) -> Int -> [Int] -> [Maybe Int]
iddfs _ _ 0 _ = []
iddfs coord path depth output =
    if Set.member coord path then
        []
    else
        let newPath = Set.insert coord path
            north = stepThen 1 output $ iddfs (move 1 coord) newPath (depth - 1)
            south = stepThen 2 (drop (length (catMaybes north)) output) $ iddfs (move 2 coord) newPath (depth - 1)
            west  = stepThen 3 (drop (length (catMaybes north) + length (catMaybes south)) output) $ iddfs (move 3 coord) newPath (depth - 1)
            east  = stepThen 4 (drop (length (catMaybes north) + length (catMaybes south) + length (catMaybes west)) output) $ iddfs (move 4 coord) newPath (depth - 1)
        in  north ++ south ++ west ++ east

search :: [Int] -> [(Int, [Maybe Int])]
search output =
    depths
    where
        depths = go 0 output
        go depth o =
            let thisDepth = iddfs (0, 0) Set.empty depth o
            in (depth, thisDepth) : go (depth + 1) (drop (length (catMaybes thisDepth)) o)

search' :: [Int] -> [Int]
search' output = map fromJust $ takeWhile isJust $ concat $ map snd $ search output

discover :: [Int] -> [Int]
discover output =
    -- 1000000 is assumed to be way more than enough search depth.
    catMaybes $ iddfs (0, 0) Set.empty 1000000 output

moveEverywhere :: [(Int, Int)] -> (Set (Int, Int), (Int, Int))
moveEverywhere io =
    (Set.insert final allButOne, oxygen)
    where
        maybeMove (c, s, o) (dir, 0) = (c, s, o)
        maybeMove (c, s, o) (dir, 1) = (move dir c, Set.insert c s, o)
        maybeMove (c, s, _) (dir, 2) =
            let newCoord = move dir c
            in  (newCoord, Set.insert c s, Just newCoord)
        (final, allButOne, Just oxygen) = foldl' maybeMove ((0, 0), Set.empty, Nothing) io

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

flood :: (Int, Int) -> Set (Int, Int) -> Int
flood oxygen =
    go 0 (Set.singleton oxygen)
    where
        go hours o2 space =
            if Set.size o2 == Set.size space then
                hours
            else
                go (hours + 1) (Set.intersection space (Set.union o2 (Set.unions (Set.map (Set.fromList . neighbors) o2)))) space

a :: IO Int
a = do
    program <- readInput
    return $ runST $ do
        memory <- initialize 1000000 program :: ST s (STArray s Int Int)
        rec output <-
                runProgram memory (ICState
                    { stdin = search' output
                    , rb = 0
                    })
        return $ fst $ fromJust $ find (\(_, xs) -> Nothing `elem` xs) $ search output

b :: IO Int
b = do
    program <- readInput
    return $ runST $ do
        memory <- initialize 1000000 program :: ST s (STArray s Int Int)
        rec output <-
                runProgram memory (ICState
                    { stdin = discover output
                    , rb = 0
                    })
        let fullInput = discover output
            (allEmptySpace, oxygen) = moveEverywhere (zip fullInput output)
        return $ flood oxygen allEmptySpace

main :: IO ()
main = do
    a >>= print
    b >>= print
