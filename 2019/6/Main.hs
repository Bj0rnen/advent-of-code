import Text.Megaparsec hiding (getInput)
import Text.Megaparsec.Char
import Data.Void
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Parser = Parsec Void String

justParse :: (Stream s, Ord e) => Parsec e s a -> s -> a
justParse p s = fromJust $ parseMaybe p s

orbit :: Parser (String, String)
orbit = do
    a <- many alphaNumChar
    char ')'
    b <- many alphaNumChar
    return (a, b)

readInput :: IO [(String, String)]
readInput = do
    s <- readFile "input.txt"
    return (justParse orbit <$> lines s)

orbits :: [(String, String)] -> Map String Int
orbits os = orbits'
    where
        orbits' = Map.fromList [if a == "COM" then (b, 1) else (b, (orbits' ! a) + 1) | (a, b) <- os]

orbitLists :: [(String, String)] -> Map String [String]
orbitLists os = orbits'
    where
        orbits' = Map.fromList [if a == "COM" then (b, [a]) else (b, b : (orbits' ! a)) | (a, b) <- os]

longestCommonTail :: Eq a => [a] -> [a] -> Int
longestCommonTail = undefined

a :: IO Int
a = sum . orbits <$> readInput

b :: IO Int
b = do
    input <- readInput
    let ls = orbitLists input
        you = ls ! "YOU"
        san = ls ! "SAN"
        youSet = Set.fromList you
        sanSet = Set.fromList san
        inter = Set.intersection youSet sanSet
    return (Set.size youSet + Set.size sanSet - 2 * Set.size inter - 2)

main :: IO ()
main = do
    a >>= print
    b >>= print
