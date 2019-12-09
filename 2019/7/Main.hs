{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
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
import Control.Monad.ST
import Control.Monad.RWS
import Data.DList hiding (head, tail)
import qualified Data.DList as DList
import Control.Monad.Trans.Class
import Data.Bifunctor
import Data.List
import Data.Ord
import Data.Proxy

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

initialize :: MArray arr Int m => [Int] -> m (arr Int Int)
initialize input =
    newListArray (0, length input - 1) input

data Memory m where
    Memory :: MArray arr Int m => arr Int Int -> Memory m

newtype Intcode m a =
    Intcode { runIntcode :: RWST (Memory m) (DList Int) [Int] m a }
    deriving newtype (Functor, Applicative, Monad, MonadWriter (DList Int), MonadState [Int])
instance Monad m => MonadReader (Memory m) (Intcode m) where
    reader f = Intcode $ reader f
    local f (Intcode ic) = Intcode $ local f ic
instance MonadTrans Intcode where
    lift = Intcode . lift

evalIntcode :: Monad m => Memory m -> [Int] -> Intcode m a -> m (a, DList Int)
evalIntcode arr inp s = evalRWST (runIntcode s) arr inp

readMem :: Monad m => Int -> Intcode m Int
readMem i = do
    Memory arr <- ask
    lift $ readArray arr i

writeMem :: Monad m => Int -> Int -> Intcode m ()
writeMem i x = do
    Memory arr <- ask
    lift $ writeArray arr i x

fetch :: Monad m => ParamMode -> Int -> Intcode m Int
fetch Position i = readMem i >>= readMem
fetch Immediate i = readMem i

getInput :: Monad m => Intcode m Int
getInput = do
    x <- gets head
    modify tail
    return x

getOutput :: Monad m => Intcode m Int
getOutput = readMem 0

binOp :: Monad m => (Int -> Int -> Int) -> Int -> ParamMode -> ParamMode -> Intcode m ()
binOp op i m1 m2 = do
    x <- fetch m1 (i + 1)
    y <- fetch m2 (i + 2)
    readMem (i + 3) >>= \ri -> writeMem ri (x `op` y)

boolOp :: Monad m => (Int -> Int -> Bool) -> Int -> ParamMode -> ParamMode -> Intcode m ()
boolOp op i m1 m2 = do
    x <- fetch m1 (i + 1)
    y <- fetch m2 (i + 2)
    readMem (i + 3) >>= \ri -> writeMem ri (if x `op` y then 1 else 0)

input :: Monad m => Int -> Intcode m ()
input i = do
    x <- getInput
    readMem (i + 1) >>= \ri -> writeMem ri x

output :: Monad m => Int -> ParamMode -> Intcode m ()
output i m1 = do
    x <- fetch m1 (i + 1)
    tell [x]

jumpIf :: Monad m => Bool -> Int -> ParamMode -> ParamMode -> Intcode m (Maybe Int)
jumpIf b i m1 m2 = do
    x <- fetch m1 (i + 1)
    if (x /= 0 && b) || (x == 0 && not b) then
        Just <$> fetch m2 (i + 2)
    else
        return Nothing

data ParamMode = Position | Immediate
    deriving (Show, Enum)

-- Param is 1-indexed
paramMode :: Int -> Int -> ParamMode
paramMode param instr = toEnum ((instr `div` (10 ^ (param+1))) `mod` 10)

run :: Monad m => Intcode m Int
run = go 0
    where
        go i = do
            instruction <- readMem i
            let opCode = instruction `mod` 100
            case opCode of
                1 -> do
                    binOp (+) i (paramMode 1 instruction) (paramMode 2 instruction)
                    go (i + 4)
                2 -> do
                    binOp (*) i (paramMode 1 instruction) (paramMode 2 instruction)
                    go (i + 4)
                3 -> do
                    input i
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
                    boolOp (<) i (paramMode 1 instruction) (paramMode 2 instruction)
                    go (i + 4)
                8 -> do
                    boolOp (==) i (paramMode 1 instruction) (paramMode 2 instruction)
                    go (i + 4)
                99 ->
                    getOutput

runProgram :: Monad m => Memory m -> [Int] -> m (DList Int)
runProgram memory input = snd <$> evalIntcode memory input run

runAmp :: Monad m => [Int] -> (Int, Memory m) -> m [Int]
runAmp inps (phase, amp) =
    DList.toList <$> runProgram amp (phase : inps)

runSTProxy :: (forall s. Proxy s -> ST s a) -> a
runSTProxy f = runST (f Proxy)

a :: IO [Int]
a = do
    input <- readInput
    let phaseSettings = permutations [0..4]
    maximum <$> forM phaseSettings \phases ->
        return $ runSTProxy \(Proxy :: Proxy s) -> do
            amps <- replicateM 5 (fmap (Memory @(STArray s)) (initialize input))
            foldM runAmp [0] (zip phases amps)

b :: IO [Int]
b = do
    input <- readInput
    let phaseSettings = permutations [5..9]
    maximumBy (comparing last) <$> forM phaseSettings \phases ->
        return $ runSTProxy \(Proxy :: Proxy s) -> do
            amps <- replicateM 5 (fmap (Memory @(STArray s)) (initialize input))
            rec feedback <- foldM runAmp (0 : feedback) (zip phases amps)
            return feedback

main :: IO ()
main = do
    a >>= print
    b >>= print
