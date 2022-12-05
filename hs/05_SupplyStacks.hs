import Data.List (elemIndex, transpose)
import Data.Maybe (fromJust, isNothing)

main :: IO ()
main = do input <- lines <$> getContents
          let (s, t) = splitAt (fromJust . elemIndex "" $ input) $ input
          let cs = map (map fromJust . dropWhile isNothing) . transpose . map (map crate . crates) . map read . init $ s
          let ps = map read . tail $ t
          putStrLn $ part1 cs ps
          putStrLn $ part2 cs ps

{- Part 1 -}

data Crate = Crate Char | Space

instance Read Crate where
  readsPrec _ ('[': x :']':t) = [(Crate x, t)]
  readsPrec _ (' ':' ':' ':t) = [(Space,   t)]
  readsPrec _ _               = []

newtype Crates = Crates { crates :: [Crate] }

instance Read Crates where
  readsPrec _ s  = [(Crates [x],   "") | (x,        "") <- reads s]
                ++ [(Crates (x:xs), u) | (x,     ' ':t) <- reads s
                                       , (Crates xs, u) <- reads t]

crate :: Crate -> Maybe Char
crate (Crate x) = Just x
crate Space     = Nothing

data Instruction = MoveFromTo Int Int Int

instance Read Instruction where
  readsPrec _ ('m':'o':'v':'e':' ':t) = [(MoveFromTo n (x-1) (y-1), w) | (n, ' ':'f':'r':'o':'m':' ':u) <- reads t
                                                                       , (x,         ' ':'t':'o':' ':v) <- reads u
                                                                       , (y,                         w) <- reads v]
  readsPrec _ _                       = []

instruct :: Show a => [[a]] -> Instruction -> [[a]]
instruct xs (MoveFromTo n x y) = let (ys, xs') = doAt x (splitAt n) xs in modifyAt y (reverse ys ++) xs'

doAt :: Int -> (a -> (b, a)) -> [a] -> (b, [a])
doAt 0 f (x:xs) = let (y, x') = f x in (y, x':xs)
doAt n f (x:xs) = let (y, xs') = doAt (n-1) f xs in (y, x:xs')

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt n f = snd . doAt n (\x -> ((), f x))

part1 :: [String] -> [Instruction] -> String
part1 cs = map head . foldl instruct cs

{- Part 2 -}

instruct' :: Show a => [[a]] -> Instruction -> [[a]]
instruct' xs (MoveFromTo n x y) = let (ys, xs') = doAt x (splitAt n) xs in modifyAt y (ys ++) xs'

part2 :: [String] -> [Instruction] -> String
part2 cs = map head . foldl instruct' cs
