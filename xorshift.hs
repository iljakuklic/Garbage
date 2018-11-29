{-# LANGUAGE NoMonomorphismRestriction #-}

module Xorshift where

import Data.Word
import Data.Bits
import Data.List

class RNG r where
  newRNG :: Word64 -> r
  random :: r -> Word64
  split :: r -> (r, r)

newtype XorShift64Star = XorShift64Star Word64 deriving (Show)

-- A0: <<a >>b <<c
-- A1: >>a <<b >>c
-- A2,3: cba
-- A4,5: acb
-- A6,7: bac

xorShiftNextEven a b c x0 = x3
  where
    xorShift x s = x `xor` (x `shift` s)
    x1 = x0 `xorShift` a
    x2 = x1 `xorShift` negate b
    x3 = x2 `xorShift` c

xorShiftNextOdd a b c = xorShiftNextEven (negate a) (negate b) (negate c)

instance RNG XorShift64Star where
  newRNG = XorShift64Star . max 1 . xor 362816527940035952
  random (XorShift64Star x) = x * 1181783497276652981
  split (XorShift64Star x0) = (XorShift64Star x1, XorShift64Star x2)
    where
      x1 = xorShiftNextOdd 5 11 45 x0
      x2 = xorShiftNextOdd 8 17 31 x0

rndAll r = rndAllGo [r]
rndAllGo :: [XorShift64Star] -> [Word64]
rndAllGo rs = fmap random rs ++ rndAllGo (concatMap rndAllSplit rs)
rndAllSplit r = let (r1, r2) = split r in [r1, r2]
rndAllFloats = fmap (\x -> floor (100 * fromIntegral x / fromIntegral (maxBound :: Word64))) . rndAll
hist = histGo . sort
histGo [] = []
histGo xs@(x:_) = let (eqx, rest) = span (==x) xs in (x, length eqx) : histGo rest

data RNDTree = RNDTree Word64 RNDTree RNDTree
rndTree r = let (r1, r2) = split r in RNDTree (random r) (rndTree r1) (rndTree r2)
