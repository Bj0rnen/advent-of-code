{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module AOC2021.Day10.Main where

import Data.Maybe (mapMaybe, fromJust)
import Data.List (foldl', sort)
import AOC2021.Common (Parser)
import Text.Megaparsec (MonadParsec(lookAhead, try, observing, eof), parse, ParseErrorBundle (ParseErrorBundle), ParseError (TrivialError), failure)
import Text.Megaparsec.Char.Lexer (charLiteral)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Set as Set
import Text.Megaparsec.Error (ErrorItem(Tokens))
import Control.Applicative (Alternative((<|>), empty))
import Text.Megaparsec.Char (char)
import Debug.Trace (traceShowId, traceShow)
import Data.Functor (($>))
import Data.Foldable (asum)

chunksParser :: Parser String
chunksParser = go []
  where
    go stack =
            char '(' *> go (')' : stack)
        <|> char '[' *> go (']' : stack)
        <|> char '{' *> go ('}' : stack)
        <|> char '<' *> go ('>' : stack)
        <|> case stack of
            [] -> empty
            c : cs -> char c *> go cs
        <|> eof $> stack

-- My original solution didn't use megaparsec:
{-
openingLexer :: Parser Char
openingLexer = char '(' <|> char '[' <|> char '{' <|> char '<'

matching :: Char -> Maybe Char
matching '(' = Just ')'
matching '[' = Just ']'
matching '{' = Just '}'
matching '<' = Just '>'
matching _ = Nothing

parseChunks :: String -> Either Char (Either String String)
parseChunks "" = Right (Right "")
parseChunks all@(c : cs) =
    case matching c of
        Nothing -> Right (Right all)
        Just closingChar -> do
            afterSubChunk <- parseChunks cs
            case afterSubChunk of
                Right "" -> Right (Left [closingChar])
                Right (x : rest)
                  | x == closingChar -> parseChunks rest
                  | otherwise        -> Left x
                Left str -> Right (Left (str ++ [closingChar]))

firstCorrupt :: String -> Maybe Char
firstCorrupt line =
    case parseChunks line of
        Left x -> Just x
        Right _ -> Nothing

autocomplete :: String -> Maybe [Char]
autocomplete line =
    case parseChunks line of
        Right (Left str) -> Just str
        _ -> Nothing
-}

firstCorrupt :: String -> Maybe Char
firstCorrupt line =
    case parse chunksParser "" line of
        Left (ParseErrorBundle ((TrivialError _ (Just (Tokens (x :| []))) _) :| []) _) -> Just x
        Right _ -> Nothing

points1 :: Char -> Int
points1 ')' = 3
points1 ']' = 57
points1 '}' = 1197
points1 '>' = 25137

solve1 :: [String] -> Int
solve1 input = sum $ map points1 $ mapMaybe firstCorrupt input

autocomplete :: String -> Maybe [Char]
autocomplete line =
    case parse chunksParser "" line of
        Left _ -> Nothing
        Right revAutocorrect -> Just revAutocorrect

points2 :: Char -> Int
points2 ')' = 1
points2 ']' = 2
points2 '}' = 3
points2 '>' = 4

middle :: [a] -> a
middle [x] = x
middle xs = middle $ tail $ init xs

solve2 :: [String] -> Int
solve2 input = middle $ sort $ map (foldl' (\acc points -> acc * 5 + points) 0 . map points2) $ mapMaybe autocomplete input

main :: IO ()
main = do
    input <- lines <$> readFile "AOC2021/Day10/input.txt"
    print $ solve1 input
    print $ solve2 input
