{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Ratio
import qualified Data.Map as M
import Control.Applicative

newtype Prob a = Prob { getProb :: [(a,Integer)] } deriving Show

instance Functor Prob where
  fmap f = Prob . fmap (\(x, p) -> (f x, p)) . getProb

instance Applicative Prob where
  pure = uniform . pure
  Prob pdf <*> Prob pdx =
    Prob $ liftA2 (\(f, pf) (x, px) -> (f x, pf * px)) pdf pdx

instance Monad Prob where
  Prob pdx >>= f =
    Prob [ (y, px * py) | (x, px) <- pdx, (y, py) <- getProb (f x) ]

instance Monoid a => Monoid (Prob a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Num a => Num (Prob a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger = pure . fromInteger

weighted = Prob
uniform xs = Prob [ (x, 1) | x <- xs ]
choose p a b = let n = numerator p in Prob [ (a, n), (b, denominator p - n) ]
choice p pa pb = choose p True False >>= \c -> if c then pa else pb
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
