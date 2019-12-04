import Data.Char
import Data.List

a :: Int
a = length $ filter (((&&) <$> any ((>= 2) . length) . group <*> (all (>= 0) . (zipWith (-) <$> tail <*> id) . map digitToInt)) . show) [240920..789857]

b :: Int
b = length $ filter (((&&) <$> any ((== 2) . length) . group <*> (all (>= 0) . (zipWith (-) <$> tail <*> id) . map digitToInt)) . show) [240920..789857]

main :: IO ()
main = do
    print a
    print b
