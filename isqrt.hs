
import Data.Word
import Data.Bits
import Data.Foldable
import Data.Traversable


-- Floor to the closest lower power of two
flp2 :: Word64 -> Word64
flp2 x = 1 `shiftL` (countLeadingZeros x `xor` 63)

-- Integer logarithm base 2
ilog2 :: Word64 -> Int
ilog2 x = countTrailingZeros (flp2 x)

ilog2up :: Word64 -> Int
ilog2up x = 32 - (countLeadingZeros (pred x) `div` 2)

-- Approximate integer square root
isqrt1 :: Word64 -> Word64
isqrt1 x = x `shiftR` ((ilog2 x + 1) `div` 2)

-- Approximate integer square root
isqrt2 :: Word64 -> Word64
isqrt2 x = 1 `shiftL` (ilog2 x `div` 2)

isqrt3 :: Word64 -> Word64
isqrt3 x = let s = ilog2up x in ((1 `shiftL` s) + (x `shiftR` s)) `div` 2

-- Appropriate

isqrt_real :: Word64 -> Word64
isqrt_real = floor . sqrt . fromIntegral

absdiff x y = max x y - min x y
isqrt1err x = absdiff (isqrt1 x) (isqrt_real x)
isqrt3err x = absdiff (isqrt3 x) (isqrt_real x)

funcs = [id, fromIntegral . ilog2, fromIntegral . ilog2up, isqrt_real, isqrt1, isqrt1err, isqrt3, isqrt3err]
vals x = map ($x) funcs

printvals n = for_ [1..n] $ print . vals
