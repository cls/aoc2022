import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do input <- map read <$> lines <$> getContents
          print . part1 $ input
          print . part2 $ input

{- Part 1 -}

data Direction = U | D | L | R

instance Read Direction where
  readsPrec _ ('U':t) = [(U, t)]
  readsPrec _ ('D':t) = [(D, t)]
  readsPrec _ ('L':t) = [(L, t)]
  readsPrec _ ('R':t) = [(R, t)]
  readsPrec _ _       = []

data Motion = Motion Direction Int

instance Read Motion where
  readsPrec _ s = [(Motion k n, u) | (k, ' ':t) <- reads s
                                   , (n,     u) <- reads t]

type Point = (Int, Int)

origin :: Point
origin = (0, 0)

expand :: Motion -> [Direction]
expand (Motion _ 0) = []
expand (Motion k n) = replicate n k

motion :: Point -> Direction -> Point
motion (x, y) U = (x, y+1)
motion (x, y) D = (x, y-1)
motion (x, y) L = (x-1, y)
motion (x, y) R = (x+1, y)

distance :: Point -> Point -> Int
distance (x0, y0) (x1, y1) = abs (x1 - x0) `max` abs (y1 - y0)

follow :: Point -> Point -> Point
follow p        q        | distance p q < 2 = p
follow (x0, y0) (x1, y1) | otherwise        = let xd = signum (x1 - x0)
                                                  yd = signum (y1 - y0)
                                               in (x0 + xd, y0 + yd)

motions :: [Motion] -> [Point]
motions = scanl motion origin . concatMap expand

follows :: [Point] -> [Point]
follows = scanl follow origin

part1 :: [Motion] -> Int
part1 = Set.size . Set.fromList . follows . motions

{- Part 2 -}

iter :: Int -> (a -> a) -> a -> a
iter 0 _ = id
iter n f = f . iter (n-1) f

part2 :: [Motion] -> Int
part2 = Set.size . Set.fromList . iter 9 follows . motions
