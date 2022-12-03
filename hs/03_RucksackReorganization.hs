import Data.Char (ord)
import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do input <- lines <$> getContents
          print . part1 $ input
          print . part2 $ input

{- Part 1 -}

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

item :: Ord a => Set a -> a
item set = let [x] = Set.toList set in x

sharedItem :: Ord a => [a] -> a
sharedItem = item . uncurry (Set.intersection `on` Set.fromList) . halve

priority :: Char -> Int
priority c | c >= 'a' && c <= 'z' = ord c - ord 'a' + 1
           | c >= 'A' && c <= 'Z' = ord c - ord 'A' + 27

part1 :: [String] -> Int
part1 = sum . map (priority . sharedItem)

{- Part 2 -}

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (ys, zs) = splitAt n xs in ys : chunksOf n zs

badge :: Ord a => [[a]] -> a
badge = item . foldr1 Set.intersection . map Set.fromList

part2 :: [String] -> Int
part2 = sum . map (priority . badge) . chunksOf 3
