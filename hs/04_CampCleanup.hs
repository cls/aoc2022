main :: IO ()
main = do input <- map (pair . read) <$> lines <$> getContents
          print . part1 $ input
          print . part2 $ input

{- Part 1 -}

data Range = Range Int Int

data Pair = Pair { pair :: (Range, Range) }

instance Read Range where
  readsPrec _ s = [(Range x y, u) | (x, '-':t) <- reads s,
                                    (y,     u) <- reads t]

instance Read Pair where
  readsPrec _ s = [(Pair (x, y), u) | (x, ',':t) <- reads s,
                                      (y,     u) <- reads t]

fullyContains :: Range -> Range -> Bool
Range a b `fullyContains` Range c d = a <= c && b >= d

fullyOverlaps :: Range -> Range -> Bool
x `fullyOverlaps` y = x `fullyContains` y || y `fullyContains` x

part1 :: [(Range, Range)] -> Int
part1 = sum . map (fromEnum . uncurry fullyOverlaps)

{- Part 2 -}

endsOverlap :: Range -> Range -> Bool
Range a b `endsOverlap` Range c d = (a <= c && b >= c) || (a <= d && b >= d)

partlyOverlaps :: Range -> Range -> Bool
x `partlyOverlaps` y = x `fullyOverlaps` y || x `endsOverlap` y

part2 :: [(Range, Range)] -> Int
part2 = sum . map (fromEnum . uncurry partlyOverlaps)
