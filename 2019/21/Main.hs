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
import Control.Monad.ST
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


isInBeam :: [Int] -> (Int, Int) -> Int
isInBeam program (x, y) = runST $ do
    memory <- initialize 10000 program :: ST s (STUArray s Int Int)
    output <-
        runProgram memory (ICState
            { stdin = [x, y]
            , rb = 0
            })
    return $ head output

solve2 :: [Int] -> Int
solve2 program =
    go (0, 99)
    where
        go (x, y) =
            if isInBeam program (x, y) == 1 then
                if isInBeam program (x + 99, y - 99) == 1 then
                    x * 10000 + (y - 99)
                else
                    go (x, y + 1)
            else
                go (x + 1, y)

strat1 :: [Bool] -> Bool
strat1 [False, _    , _    , _    ] = True
strat1 [_    , _    , _    , False] = False
strat1 [True , False, False, True ] = True
strat1 [True , False, True , True ] = True
strat1 [True , True , False, True ] = False
strat1 [True , True , True , True ] = False

strat1' [a, b, c, d] = not a || (d && not b)

strat2 :: [Bool] -> Bool
strat2 [False, _    , _    , _    ] = True
strat2 [_    , _    , _    , False] = False
strat2 [True , False, False, True ] = True
strat2 [True , False, True , True ] = True
strat2 [True , True , False, True ] = True
strat2 [True , True , True , True ] = False

strat2' [a, b, c, d] = not a || (d && (not (b && c)))
strat2'' [a, b, c, d] = not a || (d && (not b || not c))

a :: IO Int
a = do
    program <- readInput
    return $ runST $ do
        memory <- initialize 1000000 program :: ST s (STArray s Int Int)
        output <-
            runProgram memory (ICState
                { stdin = map fromEnum $ concatMap (++ "\n")
                    [ "NOT B J"
                    , "NOT C T"
                    , "OR J T"
                    , "NOT D J"
                    , "NOT J J"
                    , "AND J T"
                    , "NOT A J"
                    , "OR T J"
                    , "WALK"
                    ]
                , rb = 0
                })
        return $ last output

b :: IO Int
b = do
    undefined

main :: IO ()
main = do
    a >>= print
    --b >>= print
