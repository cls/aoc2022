import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)

main :: IO ()
main = do input <- getContents
          print . part1 . map (pair . map read) . paragraphs $ input
          print . part2 . map read . filter (not . null) . lines $ input

{- Part 1 -}

paragraphs :: String -> [[String]]
paragraphs = foldr para [[]] . lines
  where
    para "" ps = []:ps
    para ln ps = let p:ps' = ps in (ln:p):ps'

pair :: [a] -> (a, a)
pair [x, y] = (x, y)

data Value = Int Int | List [Value]

instance Read Value where
  readsPrec _ s = [(Int x,   t) | (x,  t) <- reads s]
               ++ [(List xs, t) | (xs, t) <- reads s]

instance Ord Value where
  Int x   `compare` Int y   = x `compare` y
  List xs `compare` List ys = xs `compare` ys
  List xs `compare` Int x   = xs `compare` [Int x]
  Int x   `compare` List xs = [Int x] `compare` xs

instance Eq Value where
  x == y = (x `compare` y) == EQ

part1 :: [(Value, Value)] -> Int
part1 = sum . map fst . filter (uncurry (<) . snd) . zip [1..]

{- Part 2 -}

part2 :: [Value] -> Int
part2 xs = let x1 = List [List [Int 2]]
               x2 = List [List [Int 6]]
               xs' = sort (x1:x2:xs)
               k1 = fromJust (elemIndex x1 xs') + 1
               k2 = fromJust (elemIndex x2 xs') + 1
            in k1 * k2
