import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

main :: IO ()
main = do input <- unions <$> map (build . read) <$> lines <$> getContents
          print . part1 $ input
          print . part2 $ input

{- Part 1 -}

type Point = (Int, Int)

newtype Wall = Wall { points :: [Point] }

instance Read Wall where
  readsPrec _ s = [(Wall [(x,y)], u) | (x, ',':t) <- reads s
                                     , (y,     u) <- reads t]
               ++ [(Wall ((x,y):ws), v) | (x,             ',':t) <- reads s
                                        , (y, ' ':'-':'>':' ':u) <- reads t
                                        , (Wall ws,           v) <- reads u]

signumFromTo :: Int -> Int -> [Int]
signumFromTo a b = [a, a + signum (b - a) .. b]

pairs :: [a] -> [(a, a)]
pairs (x:xs@(y:_)) = (x,y) : pairs xs
pairs _            = []

wall :: Point -> Point -> [Point]
wall (x0,y0) (x1,y1) | x0 == x1  = [(x0, y) | y <- signumFromTo y0 y1]
                     | otherwise = [(x, y0) | x <- signumFromTo x0 x1]

type Cave = IntMap IntSet

plot :: Point -> Cave
plot (x,y) = IntMap.singleton x $ IntSet.singleton y

union :: Cave -> Cave -> Cave
union = IntMap.unionWith IntSet.union

unions :: [Cave] -> Cave
unions = foldr union IntMap.empty

build :: Wall -> Cave
build = unions . map plot . concatMap (uncurry wall) . pairs . points

data Result = Settled Point | Blocked | Abyss

type Ground = Int -> Result

sand :: Ground -> Point -> Cave -> Result
sand g (x,y) cave = case IntSet.lookupGE y . IntMap.findWithDefault IntSet.empty x $ cave of
                      Nothing             -> g x
                      Just y' | y == y'   -> Blocked
                              | otherwise -> case sand g (x-1, y') cave of
                                               Abyss     -> Abyss
                                               Settled l -> Settled l
                                               Blocked   -> case sand g (x+1, y') cave of
                                                              Abyss     -> Abyss
                                                              Settled r -> Settled r
                                                              Blocked   -> Settled (x, y'-1)

origin :: Point
origin = (500, 0)

sands :: Ground -> Cave -> Cave
sands g cave = case sand g origin cave of
                 Settled p -> sands g . union cave . plot $ p
                 _         -> cave

volume :: Cave -> Int
volume = IntMap.foldr (+) 0 . IntMap.map IntSet.size

sandVolume :: Ground -> Cave -> Int
sandVolume g cave = let old = volume $ cave
                        new = volume . sands g $ cave
                     in new - old

part1 :: Cave -> Int
part1 = sandVolume (const Abyss)

{- Part 2 -}

part2 :: Cave -> Int
part2 cave = let ground x = let y = IntMap.foldr max 0 . IntMap.map IntSet.findMax $ cave in Settled (x, y+1)
              in sandVolume ground cave
