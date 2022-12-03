main :: IO ()
main = do input <- lines <$> getContents
          print . part1 . map read $ input
          print . part2 . map read $ input

{- Part 1 -}

data Shape = Rock | Paper | Scissors
  deriving (Eq)

instance Read Shape where
  readsPrec _ (c:t) | c == 'A' || c == 'X' = [(Rock,     t)]
  readsPrec _ (c:t) | c == 'B' || c == 'Y' = [(Paper,    t)]
  readsPrec _ (c:t) | c == 'C' || c == 'Z' = [(Scissors, t)]
  readsPrec _ _                            = []

beat :: Shape -> Shape
beat Rock     = Paper
beat Paper    = Scissors
beat Scissors = Rock

beats :: Shape -> Shape -> Bool
p `beats` r = p == beat r

data PlayAndResponse = PlayAndResponse Shape Shape

instance Read PlayAndResponse where
  readsPrec _ s = [(PlayAndResponse p r, u) | (p, ' ':t) <- reads s,
                                              (r,     u) <- reads t]

data Outcome = Lose | Draw | Win

outcome :: PlayAndResponse -> Outcome
outcome (PlayAndResponse p r) | p `beats` r = Lose
                              | p == r      = Draw
                              | otherwise   = Win

class Score a where
  score :: a -> Int

instance Score Shape where
  score Rock     = 1
  score Paper    = 2
  score Scissors = 3

instance Score Outcome where
  score Lose = 0
  score Draw = 3
  score Win  = 6

instance Score PlayAndResponse where
  score x@(PlayAndResponse _ r) = score (outcome x) + score r

instance Score a => Score [a] where
  score = sum . map score

part1 :: [PlayAndResponse] -> Int
part1 = score

{- Part 2 -}

data PlayAndOutcome = PlayAndOutcome Shape Outcome

instance Read Outcome where
  readsPrec _ ('X':t) = [(Lose, t)]
  readsPrec _ ('Y':t) = [(Draw, t)]
  readsPrec _ ('Z':t) = [(Win,  t)]

instance Read PlayAndOutcome where
  readsPrec _ s = [(PlayAndOutcome p o, u) | (p, ' ':t) <- reads s,
                                             (o,     u) <- reads t]

loseTo :: Shape -> Shape
loseTo Rock     = Scissors
loseTo Paper    = Rock
loseTo Scissors = Paper

response :: PlayAndOutcome -> Shape
response (PlayAndOutcome p Lose) = loseTo p
response (PlayAndOutcome p Draw) = p
response (PlayAndOutcome p Win)  = beat p

instance Score PlayAndOutcome where
  score x@(PlayAndOutcome _ o) = score o + score (response x)

part2 :: [PlayAndOutcome] -> Int
part2 = score
