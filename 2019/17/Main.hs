{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}

import Text.Megaparsec hiding (getInput)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor
import Data.Void
import Data.Maybe
import Data.Array
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


intersections :: Array (Int, Int) Char -> [(Int, Int)]
intersections m =
    filter isIntersection $ range bnds
    where
        bnds@((0, 0), (maxX, maxY)) = bounds m
        isIntersection (x, y)
            | x == 0 ||
              y == 0 ||
              x == maxX ||
              y == maxY =
                False
            | m ! (x, y) == '#' &&
              m ! (x - 1, y) == '#' &&
              m ! (x + 1, y) == '#' &&
              m ! (x, y - 1) == '#' &&
              m ! (x, y + 1) == '#' =
                True
            | otherwise = False

alignmentParameter :: (Int, Int) -> Int
alignmentParameter (x, y) = x * y

plot :: Array (Int, Int) Char -> IO ()
plot m =
    forM_ [snd (fst (bounds m))..snd (snd (bounds m))] $ \y -> do
        forM_ [fst (fst (bounds m))..fst (snd (bounds m))] $ \x ->
            putChar (m ! (x, y))
        putStrLn ""

a :: IO Int
a = do
    program <- readInput
    return $ runST $ do
        memory <- initialize 1000000 program :: ST s (STArray s Int Int)
        output <-
            runProgram memory (ICState
                { stdin = []
                , rb = 0
                })
        let text = map (toEnum @Char) output
            ls = init $ lines text
            rows = length ls
            cols = length $ ls !! 0
            m = listArray ((0, 0), (cols - 1, rows - 1)) (concat $ transpose ls)
            is = intersections m
            aps = map alignmentParameter is
        return $ sum aps


routine = "A,A,B,C,A,C,A,B,C,B"
programA = "R,12,L,8,R,6"
programB = "R,12,L,6,R,6,R,8,R,6"
programC = "L,8,R,8,R,6,R,12"
--------------------------------
-- R,12,L,8,R,6, R,12,L,8,R,6, R,12,L,6,R,6,R,8,R,6, L,8,R,8,R,6,R,12, R,12,L,8,R,6, L,8,R,8,R,6,R,12, R,12,L,8,R,6, R,12,L,6,R,6,R,8,R,6, L,8,R,8,R,6,R,12, R,12,L,6,R,...
b :: IO Int
b = do
    program <- readInput
    return $ runST $ do
        memory <- initialize 1000000 program :: ST s (STArray s Int Int)
        writeArray memory 0 2
        output <-
            runProgram memory (ICState
                { stdin = map fromEnum $ concatMap (++ "\n") [routine, programA, programB, programC, "n"]
                , rb = 0
                })
        return $ last output

main :: IO ()
main = do
    a >>= print
    b >>= print
