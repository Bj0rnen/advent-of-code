{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

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

initialize :: MArray a Int m => [Int] -> Int -> Int -> m (a Int Int)
initialize input noun verb = do
    arr <- newListArray (0, size-1) input
    writeArray arr 1 noun
    writeArray arr 2 verb
    return arr
    where
        size = length input

binOp :: MArray a Int m => (Int -> Int -> Int) -> Int -> a Int Int -> m ()
binOp op i arr = do
    x <- readArray arr (i + 1) >>= readArray arr
    y <- readArray arr (i + 2) >>= readArray arr
    readArray arr (i + 3) >>= \zi -> writeArray arr zi (x `op` y)

runProgram :: MArray a Int m => a Int Int -> m (a Int Int)
runProgram arr = go 0
    where
        go i = do
            opCode <- readArray arr i
            case opCode of
                1 -> do
                    binOp (+) i arr
                    go (i + 4)
                2 -> do
                    binOp (*) i arr
                    go (i + 4)
                99 -> return arr

getOutput :: MArray a Int m => a Int Int -> m Int
getOutput arr = readArray arr 0

a :: IO Int
a = do
    input <- readInput
    return $ runST $ do
        arr <- initialize input 12 2 :: ST s (STUArray s Int Int)
        runProgram arr
        getOutput arr

allInits :: MArray a Int m => [Int] -> m [a Int Int]
allInits input =
    forM (range ((0, 0), (99, 99))) $ \(noun, verb) ->
        initialize input noun verb

b :: IO Int
b = do
    input <- readInput
    return $ runST $ do
        inits <- allInits input :: ST s [STUArray s Int Int]
        results <- forM inits $ \arr -> do
            runProgram arr
        [theResult] <- filterM (\arr -> (== 19690720) <$> getOutput arr) results
        (\noun verb -> 100 * noun + verb) <$> readArray theResult 1 <*> readArray theResult 2
