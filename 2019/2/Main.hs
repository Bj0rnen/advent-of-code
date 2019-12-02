{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}

import Text.Megaparsec
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

type Parser = Parsec Void String

justParse :: (Stream s, Ord e) => Parsec e s a -> s -> a
justParse p s = fromJust $ parseMaybe p s

integers :: Parser [Int]
integers = do
    n <- L.decimal
    ns <-   char ',' *> integers
        <|> (eof <|> void newline) *> return []
    return (n : ns)

readInput :: IO [Int]
readInput = do
    s <- readFile "input.txt"
    return $ justParse integers s

initialize :: MArray arr Int m => [Int] -> Int -> Int -> m (arr Int Int)
initialize input noun verb = do
    arr <- newListArray (0, size-1) input
    writeArray arr 1 noun
    writeArray arr 2 verb
    return arr
    where
        size = length input

newtype Intcode arr m a =
    Intcode { runIntcode :: (StateT (arr Int Int) m a) }
    deriving newtype (Functor, Applicative, Monad, MonadState (arr Int Int), MonadTrans)

evalIntcode :: MArray arr Int m => arr Int Int -> Intcode arr m a -> m a
evalIntcode arr s = evalStateT (runIntcode s) arr

readMem :: MArray arr Int m => Int -> Intcode arr m Int
readMem i = do
    arr <- get
    lift $ readArray arr i

writeMem :: MArray arr Int m => Int -> Int -> Intcode arr m ()
writeMem i x = do
    arr <- get
    lift $ writeArray arr i x

binOp :: MArray arr Int m => (Int -> Int -> Int) -> Int -> Intcode arr m ()
binOp op i = do
    x <- readMem (i + 1) >>= readMem
    y <- readMem (i + 2) >>= readMem
    readMem (i + 3) >>= \ri -> writeMem ri (x `op` y)

getOutput :: MArray arr Int m => Intcode arr m Int
getOutput = readMem 0

runProgram :: MArray arr Int m => Intcode arr m Int
runProgram = go 0
    where
        go i = do
            opCode <- readMem i
            case opCode of
                1 -> do
                    binOp (+) i
                    go (i + 4)
                2 -> do
                    binOp (*) i
                    go (i + 4)
                99 ->
                    getOutput

a :: IO Int
a = do
    input <- readInput
    return $ runST do
        arr <- initialize input 12 2 :: ST s (STUArray s Int Int)
        evalIntcode arr runProgram

b :: IO Int
b = do
    input <- readInput
    return $ runST do
        results <- forM (range ((0, 0), (99, 99))) $ \(noun, verb) -> do
            arr <- initialize input noun verb :: ST s (STUArray s Int Int)
            evalIntcode arr do
                output <- runProgram
                return
                    if output == 19690720 then
                        Just (100 * noun + verb)
                    else
                        Nothing
        return $ catMaybes results !! 0
