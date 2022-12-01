import Data.List (sortBy)

paragraphs :: String -> [[String]]
paragraphs = foldr para [[]] . lines
  where
    para ""    ps  =     []:ps
    para ln (p:ps) = (ln:p):ps

main :: IO ()
main = do input <- getContents
          let elves = map (map read) . paragraphs $ input :: [[Int]]
          print . maximum . map sum $ elves
          print . sum . take 3 . sortBy (flip compare) . map sum $ elves
