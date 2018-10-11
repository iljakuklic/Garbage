{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Int
import Data.Bits
import Data.Ratio

type Mantissa = Integer
type Exponent = Int

-- [[APFloat m e]] = m * 2^e
data APFloat = APFloat Mantissa Exponent deriving Show

-- Calculate mantissa so that exponent is e', may lose precision
apfShiftTo (APFloat m e) e' = shift m (e - e')
apfAlign a@(APFloat ma ea) b@(APFloat mb eb)
  = let e = min ea eb in (apfShiftTo a e, apfShiftTo b e, e)
apfWithAligned f a b = let (a', b', e) = apfAlign a b in APFloat (f a' b') e

instance Eq APFloat where
  a == b = compare a b == EQ

instance Ord APFloat where
  compare a b = let (a', b', _) = apfAlign a b in compare a' b'

instance Real APFloat where
  toRational (APFloat m e) | e >= 0 = (m `shift` e) % 1
  toRational (APFloat m e) | otherwise = m % (2^(negate e))

instance Fractional APFloat where
  recip (APFloat 1 e) = APFloat 1 (-e)
  recip x = error "recip: not implemented"
  fromRational r | numerator r == 1 && denominator r == 2 = APFloat 1 (-1)
  fromRational r = error "fromRational: not implemented"

instance RealFrac APFloat where
  properFraction (APFloat m e) | e >= 0
    = (fromInteger (m `shift` e), 0)
  properFraction x@(APFloat m e) | otherwise
    = let ip = fromInteger (m `shift` e) in (ip, x - ip)

instance Bits APFloat where
  (.&.) = apfWithAligned (.&.)
  (.|.) = apfWithAligned (.|.)
  xor = error "xor: cannot be implemented"
  complement (APFloat m e) = error "complement: cannot be implemented"
  shift (APFloat m e) n = APFloat m (e + n)
  rotate = error "rotate: cannot be implemented"
  isSigned _ = True
  testBit (APFloat m e) n | n < e = False
  testBit (APFloat m e) n | otherwise = testBit m (n - e)
  bitSize (APFloat m e) = error "bitSize: deprecated"
  bitSizeMaybe (APFloat m e) = fmap (e+) (bitSizeMaybe m)
  bit n = APFloat 1 n
  popCount (APFloat m _) = popCount m

instance Num APFloat where
  (+) = apfWithAligned (+)
  (-) = apfWithAligned (-)
  APFloat ma ea * APFloat mb eb = APFloat (ma * mb) (ea + eb)
  abs (APFloat m e) = APFloat (abs m) e
  signum (APFloat m e) = APFloat (signum m) 0
  fromInteger x = APFloat x 0


