readModules :: IO [Integer]
readModules = fmap read . lines <$> readFile "input.txt"

fuel :: Integer -> Integer
fuel x = (x `div` 3) - 2

fuelsList :: Integer -> [Integer]
fuelsList = takeWhile (> 0) . drop 1 . iterate fuel

combinedFuel :: Integer -> Integer
combinedFuel = sum . fuelsList

a :: IO Integer
a = do
    weights <- readModules
    let fuels = fuel <$> weights
    return $ sum fuels

b :: IO Integer
b = do
    weights <- readModules
    let combinedFuels = combinedFuel <$> weights
    return $ sum combinedFuels

main :: IO ()
main = do
    a >>= print
    b >>= print
