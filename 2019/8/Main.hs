import Data.List
import Data.Ord

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

split :: Int -> [a] -> [[a]]
split i =
    unfoldr
        (\inp ->
            if null inp then
                Nothing
            else
                Just (splitAt i inp))

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\z -> if z == x then y else z)

a :: IO Int
a = do
    [input] <- lines <$> readFile "input.txt"
    let layers = split (25 * 6) input
        zerosAndAnswers =
            map (\layer ->
                    (count '0' layer, count '1' layer * count '2' layer))
                layers
    return $ snd $ minimumBy (comparing fst) zerosAndAnswers

b :: IO [[Char]]
b = do
    [input] <- lines <$> readFile "input.txt"
    let layers = split (25 * 6) input
        decoded = foldl' (zipWith (\x y -> if x == '2' then y else x)) (repeat '2') layers
    return $ split 25 decoded

main :: IO ()
main = do
    a >>= print
    b >>= mapM_ (putStrLn . replace '1' '█' . replace '0' ' ')
