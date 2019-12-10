import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ratio

-- for each asteroid a
--     sort other asteroids by distance from a (pythagoras)
--     store in map with rational angle as key

data Leaning =
      Inside
    | Up
    | Down
    | R Rational
    | L Rational
    deriving (Eq, Ord)

leaning :: (Integer, Integer) -> (Integer, Integer) -> Leaning
leaning p@(pr, pc) q@(qr, qc)
    | p == q = Inside
    | pc == qc && pr > qr = Up
    | pc == qc && pr < qr = Down
    | pc < qc = R ((qr - pr) % (qc - pc))
    | pc > qc = L ((qr - pr) % (qc - pc))

numVisible :: [(Integer, Integer)] -> (Integer, Integer) -> Int
numVisible ps p = length $ Set.fromList (filter (/= Inside) $ map (leaning p) ps)

a :: IO Int
a = do
    input <- lines <$> readFile "input.txt"
    let points =
            map fst $
            filter ((== '#') . snd) $
            concat $
                zipWith (\row -> zipWith (\col ch -> ((row, col), ch)) [0..]) [0..] input
    return $ maximum $ map (numVisible points) points

b :: IO Int
b = undefined

main :: IO ()
main = do
    a >>= print
    --b >>= print
