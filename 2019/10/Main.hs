import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio
import Data.Bifunctor
import Data.List
import Data.Ord

data Leaning =
      U
    | R Rational
    | D
    | L Rational
    deriving (Eq, Ord)

leaning :: (Integer, Integer) -> (Integer, Integer) -> Leaning
leaning p@(px, py) q@(qx, qy)
    | p == q = error "Can't compute leaning with self"
    | px == qx && py > qy = U
    | px < qx = R ((qy - py) % (qx - px))
    | px == qx && py < qy = D
    | px > qx = L ((qy - py) % (qx - px))

distSqr :: (Integer, Integer) -> (Integer, Integer) -> Integer
distSqr (x1, y1) (x2, y2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

detected :: (Integer, Integer) -> [(Integer, Integer)] -> Map Leaning (Integer, Integer)
detected p ps =
    Map.fromListWith
        (\q1 q2 -> if distSqr p q1 < distSqr p q2 then q1 else q2)
        (map (\q -> (leaning p q, q)) ps)

numVisible :: (Integer, Integer) -> [(Integer, Integer)] -> Int
numVisible p = length . detected p

every :: [a] -> [(a, [a])]
every [] = []
every (x : xs) = (x, xs) : map (second ((:) x)) (every xs)

bestLocation :: [(Integer, Integer)] -> (Integer, Integer)
bestLocation points =
    fst $ maximumBy (comparing (uncurry numVisible)) (every points)

vaporizationOrder :: (Integer, Integer) -> Set (Integer, Integer) -> [(Integer, Integer)]
vaporizationOrder p ps =
    case Set.null ps of
        True -> []
        False ->
            firstBatch ++ vaporizationOrder p rest
            where
                firstBatch = Map.elems (detected p (Set.toList ps))
                rest = Set.difference ps (Set.fromList firstBatch)

a :: IO Int
a = do
    input <- lines <$> readFile "input.txt"
    let points =
            map fst $
            filter ((== '#') . snd) $
            concat $
                zipWith (\y -> zipWith (\x c -> ((x, y), c)) [0..]) [0..] input
    return $ maximum $ map (uncurry numVisible) (every points)

b :: IO Integer
b = do
    input <- lines <$> readFile "input.txt"
    let points =
            map fst $
            filter ((== '#') . snd) $
            concat $
                zipWith (\y -> zipWith (\x c -> ((x, y), c)) [0..]) [0..] input
        best = bestLocation points
        ordered = vaporizationOrder best (Set.fromList (filter (/= best) points))
        (x, y) = ordered !! 199
    --putStrLn $ "Best = " ++ show best
    --mapM_ print (zip [1..] ordered)
    return $ x * 100 + y

main :: IO ()
main = do
    a >>= print
    b >>= print
