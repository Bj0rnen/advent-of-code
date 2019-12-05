{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}

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
import Control.Monad.State
import Control.Monad.Writer
import Data.DList hiding (head, tail)
import Control.Monad.Trans.Class
import Data.Bifunctor

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

newtype Intcode arr m a =
    Intcode { runIntcode :: WriterT (DList Int) (StateT (arr Int Int, [Int]) m) a }
    deriving newtype (Functor, Applicative, Monad, MonadWriter (DList Int), MonadState (arr Int Int, [Int]))
instance MonadTrans (Intcode arr) where
    lift = Intcode . lift . lift

evalIntcode :: MArray arr Int m => arr Int Int -> [Int] -> Intcode arr m a -> m (a, DList Int)
evalIntcode arr inp s = evalStateT (runWriterT (runIntcode s)) (arr, inp)

readMem :: MArray arr Int m => Int -> Intcode arr m Int
readMem i = do
    arr <- gets fst
    lift $ readArray arr i

writeMem :: MArray arr Int m => Int -> Int -> Intcode arr m ()
writeMem i x = do
    arr <- gets fst
    lift $ writeArray arr i x

fetch :: MArray arr Int m => ParamMode -> Int -> Intcode arr m Int
fetch Position i = readMem i >>= readMem
fetch Immediate i = readMem i

getInput :: MArray arr Int m => Intcode arr m Int
getInput = do
    x <- gets (head . snd)
    modify (second tail)
    return x

getOutput :: MArray arr Int m => Intcode arr m Int
getOutput = readMem 0

binOp :: MArray arr Int m => (Int -> Int -> Int) -> Int -> ParamMode -> ParamMode -> Intcode arr m ()
binOp op i m1 m2 = do
    x <- fetch m1 (i + 1)
    y <- fetch m2 (i + 2)
    readMem (i + 3) >>= \ri -> writeMem ri (x `op` y)

boolOp :: MArray arr Int m => (Int -> Int -> Bool) -> Int -> ParamMode -> ParamMode -> Intcode arr m ()
boolOp op i m1 m2 = do
    x <- fetch m1 (i + 1)
    y <- fetch m2 (i + 2)
    readMem (i + 3) >>= \ri -> writeMem ri (if x `op` y then 1 else 0)

input :: MArray arr Int m => Int -> Intcode arr m ()
input i = do
    x <- getInput
    readMem (i + 1) >>= \ri -> writeMem ri x

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

data ParamMode = Position | Immediate
    deriving (Show, Enum)

-- Param is 1-indexed
paramMode :: Int -> Int -> ParamMode
paramMode param instr = toEnum ((instr `div` (10 ^ (param+1))) `mod` 10)

runProgram :: MArray arr Int m => Intcode arr m Int
runProgram = go 0
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

a :: IO (Int, DList Int)
a = do
    input <- readInput
    return $ runST $ do
        arr <- initialize input :: ST s (STUArray s Int Int)
        evalIntcode arr [1] runProgram

b :: IO (Int, DList Int)
b = do
    input <- readInput
    return $ runST $ do
        arr <- initialize input :: ST s (STUArray s Int Int)
        evalIntcode arr [5] runProgram

main :: IO ()
main = do
    a >>= print
    b >>= print
