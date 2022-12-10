main :: IO ()
main = do input <- map read <$> lines <$> getContents
          print  . part1 $ input
          putStr . part2 $ input

{- Part 1 -}

data Instruction = Addx Int | Noop

instance Read Instruction where
  readsPrec _ ('a':'d':'d':'x':' ':t) = [(Addx x, u) | (x, u) <- reads t]
  readsPrec _ ('n':'o':'o':'p':    t) = [(Noop,   t)]
  readsPrec _ _                       = []

cycles :: Instruction -> [Int]
cycles (Addx x) = [0, x]
cycles Noop     = [0]

runCycles :: [Instruction] -> [Int]
runCycles = scanl (+) 1 . concatMap cycles

signalStrengths :: [Instruction] -> [Int]
signalStrengths = zipWith (*) [1..] . runCycles

takeEvery :: Int -> [a] -> [a]
takeEvery n (x:xs) = x : takeEvery n (drop (n-1) xs)
takeEvery n []     = []

part1 :: [Instruction] -> Int
part1 = sum . takeEvery 40 . drop 19 . signalStrengths

{- Part 2 -}

sprite :: Int -> Int -> Bool
sprite n x = let p = n `mod` 40 in x >= p - 1 && x <= p + 1

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (ys, zs) = splitAt n xs in ys : chunksOf n zs

pixel :: Bool -> Char
pixel True  = '#'
pixel False = '.'

render :: [Bool] -> String
render = unlines . chunksOf 40 . map pixel

part2 :: [Instruction] -> String
part2 = render . zipWith sprite [0..] . init . runCycles
