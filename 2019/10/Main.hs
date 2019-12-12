import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio

-- for each asteroid a
--     sort other asteroids by distance from a (pythagoras)
--     store in map with rational angle as key

data Leaning =
      U
    | R Rational
    | D
    | L Rational
    deriving (Eq, Ord)

leaning :: (Integer, Integer) -> (Integer, Integer) -> Leaning
leaning p@(pr, pc) q@(qr, qc)
    | p == q = error "Can't compute leaning with self"
    | pc == qc && pr > qr = U
    | pc < qc = R ((qr - pr) % (qc - pc))
    | pc == qc && pr < qr = D
    | pc > qc = L ((qr - pr) % (qc - pc))

detected :: [(Integer, Integer)] -> (Integer, Integer) -> Map Leaning (Integer, Integer)
detected ps p = Map.fromList (map (\q -> (leaning p q, q)) ps)

numVisible :: [(Integer, Integer)] -> (Integer, Integer) -> Int
numVisible ps = length . detected ps

bestLocation :: [(Integer, Integer)] -> (Integer, Integer)
bestLocation ps = map (\p -> filter (/= p) points) ps

vaporizationOrder :: (Integer, Integer) -> [(Integer, Integer)] -> [(Integer, Integer)]
vaporizationOrder = undefined

a :: IO Int
a = do
    input <- lines <$> readFile "input.txt"
    let points =
            map fst $
            filter ((== '#') . snd) $
            concat $
                zipWith (\y -> zipWith (\x c -> ((y, x), c)) [0..]) [0..] input
    return $ maximum $ map (\p -> numVisible (filter (/= p) points) p) points

b :: IO Integer
b = do
    input <- lines <$> readFile "input.txt"
    let points =
            map fst $
            filter ((== '#') . snd) $
            concat $
                zipWith (\y -> zipWith (\x c -> ((y, x), c)) [0..]) [0..] input
        best = bestLocation points
    return $ (\(y, x) -> x*100 + y) $ vaporizationOrder best (filter (/= best) points) !! 199

main :: IO ()
main = do
    a >>= print
    b >>= print
