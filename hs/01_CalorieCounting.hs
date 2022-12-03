import Data.List (sortBy)

main :: IO ()
main = do input <- map (map read) <$> paragraphs <$> getContents
          print . part1 $ input
          print . part2 $ input

{- Part 1 -}

paragraphs :: String -> [[String]]
paragraphs = foldr para [[]] . lines
  where
    para "" ps = []:ps
    para ln ps = let p:ps' = ps in (ln:p):ps'

part1 :: [[Int]] -> Int
part1 = maximum . map sum

{- Part 2 -}

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

part2 :: [[Int]] -> Int
part2 = sum . take 3 . sortDesc . map sum
