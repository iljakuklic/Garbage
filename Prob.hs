
import Data.Ratio
import qualified Data.Map as M

newtype Prob a = Prob { getProb :: [(a, Integer)] } deriving (Show)

instance Functor Prob where
  fmap f = Prob . fmap (\(x, p) -> (f x, p)) . getProb

instance Applicative Prob where
  pure x = uniform [x]
  Prob pfs <*> Prob pxs = Prob [ (f x, pf * px) | (f, pf) <- pfs, (x, px) <- pxs ]

instance Monad Prob where
  Prob pxs >>= pfs = Prob [ (y, px * py) | (x, px) <- pxs, (y, py) <- getProb (pfs x) ]

weighted = Prob
uniform xs = Prob [ (x, 1) | x <- xs ]
choose p a b = let n = numerator p in Prob [ (n, a), (denominator p - n, b) ]
normalize :: Ord a => Prob a -> Prob a
normalize = Prob . M.toList . M.fromListWith (+) . getProb
totalWeight = sum . fmap snd . getProb

d6 = uniform [1..6]
dF = uniform [-1,0,1]
coin = uniform [0,1]

printProb ps' = mapM_ (putStrLn . entry) ps
  where
    pps@(Prob ps) = normalize ps'
    total = totalWeight pps
    maxp = maximum [ p | (_, p) <- ps ]
    barWidth :: (Ord a, Num a) => a
    barWidth = min (fromInteger maxp) 60
    barLen p = fromInteger ((p * barWidth) `div` maxp) :: Int
    bar p = take barWidth (replicate (barLen p) '#' ++ repeat ' ')
    entry (x, p) = " " ++ bar p ++ " | " ++ show x
