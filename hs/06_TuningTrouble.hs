import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do input <- takeWhile (/= '\n') <$> getContents
          print . part1 $ input
          print . part2 $ input

{- Part 1 -}

isMarker :: Int -> String -> Bool
isMarker n xs = (Set.size . Set.fromList . take n $ xs) == n

findMarker :: Int -> String -> Int
findMarker n xs | isMarker n xs = n
                | otherwise     = findMarker n (tail xs) + 1

part1 :: String -> Int
part1 = findMarker 4

{- Part 2 -}

part2 :: String -> Int
part2 = findMarker 14
