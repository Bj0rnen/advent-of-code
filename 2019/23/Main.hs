{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
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
import Data.Array.IO
import Control.Monad
import Control.Monad.ST.Lazy
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Class
import Data.Bifunctor
import Data.List
import Data.Ord
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import System.IO.Unsafe
import Control.DeepSeq
import Prelude hiding (log)
import Pipes
import qualified Pipes.Prelude as P
import Pipes.Concurrent

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
    { rb   :: Int
    --, pc    :: Int
    }

newtype Intcode arr m a =
    Intcode { runIntcode :: ReaderT (arr Int Int) (StateT ICState (Pipe Int Int m)) a }
    deriving newtype (Functor, Applicative, Monad, MonadReader (arr Int Int), MonadState ICState)
instance MonadTrans (Intcode arr) where
    lift = Intcode . lift . lift . lift

evalIntcode :: MArray arr Int m => arr Int Int -> ICState -> Intcode arr m a -> Pipe Int Int m a
evalIntcode arr inp s = evalStateT (runReaderT (runIntcode s) arr) inp

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
    x <- Intcode $ lift $ lift await
    write m1 (i + 1) x

output :: MArray arr Int m => Int -> ParamMode -> Intcode arr m ()
output i m1 = do
    x <- fetch m1 (i + 1)
    Intcode $ lift $ lift $ yield x

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

run :: MArray arr Int m => Intcode arr m ()
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
                    return ()

runProgram :: MArray arr Int m => arr Int Int -> ICState -> Pipe Int Int m ()
runProgram memory icState = evalIntcode memory icState run


triples :: [a] -> [(a, a, a)]
triples [] = []
triples (x : y : z : xs) = (x, y, z) : triples xs

sendMessage :: MonadIO m => Array Int (Output Int) -> Consumer' Int m ()
sendMessage outputs = loop
    where
        loop = do
            i <- await
            let output = outputs ! i
            x <- await
            y <- await
            if i == 255 then
                liftIO $ putStrLn $ "The answer is " ++ show y
            else do
                couldSend <- liftIO $ atomically $ do
                    (&&) <$> send output x <*> send output y
                when couldSend loop

minusOnes :: MonadIO m => Producer Int m ()
minusOnes = forever do
    liftIO $ threadDelay 1000000
    yield (-1)


a :: IO [[Int]]
a = do
    program <- readInput
    (outputs', inputs', seals) <- unzip3 <$> replicateM 50 (spawn' unbounded)
    let outputs = listArray (0, 49) outputs'
        inputs = listArray (0, 49) inputs'

    minusOners <- forM [0..49] \i -> async $ runEffect $ minusOnes >-> toOutput (outputs ! i)

    xs <- forConcurrently [0..49] \i -> do
        memory <- initialize 100000 program :: IO (IOArray Int Int)
        runEffect $
            (yield i >> fromInput (inputs ! i))
            >->
            runProgram memory (ICState { rb = 0 })
            >->
            sendMessage outputs
    atomically $ sequence seals
    return []

b :: IO Int
b = do
    undefined

main :: IO ()
main = do
    a >>= print
    --b >>= print
