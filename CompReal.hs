
import Data.Int
import Data.List

-- Signed digits real numbers (base 2^63)
data SDReal = SDReal { mant :: [Int64], expt :: Integer } deriving (Show)

instance Eq SDReal where
  x == y = compare x y == EQ

instance Ord SDReal where
  compare xr yr = go 0 xm ym
    where
      (_, xm, ym) = align xr yr
      go c [] [] = compare 0 c
      go c [] ys = go c [0] ys
      go c xs [] = go c xs [0]
      go c (x:xs) (y:ys) = let r = y - x + c in
        if abs r <= 1 then go (r * sdBase) xs ys
        else compare 0 r

instance Num SDReal where
  negate (SDReal m e) = SDReal (fmap negate m) e
  abs r = if r >= 0 then r else negate r
  signum r = case compare r 0 of
    GT -> 1
    EQ -> 0
    LT -> -1
  fromInteger = sdrFromQ . fromInteger

instance Fractional SDReal where
  fromRational = sdrFromQ

instance Real SDReal where
  -- approximate conversion to rational

instance RealFrac SDReal where
  properFraction (SDReal m e) = (intPart e m 0, normalize fracPart)
    where
      eMant = fromIntegral (max 0 e)
      fracPart = SDReal (drop eMant m) (min 0 e)
      intPart :: Integral b => Integer -> [Int64] -> b -> b
      intPart ee _ x | ee <= 0 = x
      intPart ee [] x = x * (sdBase ^ ee)
      intPart ee (m:ms) x = intPart (pred ee) ms (x * sdBase + fromIntegral m)

instance Floating SDReal where

sdBase :: Num a => a
sdBase = 10 -- 2^62

addc64 :: Int64 -> Int64 -> (Int64, Int64)
addc64 x y = quotRem (x + y) sdBase

-- TODO this is wrong
addMant :: Int64 -> [Int64] -> [Int64] -> [Int64]
addMant z xs [] = z : xs
addMant z [] ys = z : ys
addMant z (x:xs) (y:ys) = (z + c) : addMant z' xs ys
  where (c, z') = addc64 x y

addsd :: SDReal -> SDReal -> SDReal
addsd x y = normalize (SDReal (addMant 0 xm ym) (succ e'))
  where (e', xm, ym) = align x y

align :: SDReal -> SDReal -> (Integer, [Int64], [Int64])
align (SDReal xm xe) (SDReal ym ye) = (e', pad xe xm, pad ye ym)
  where
    e' = max xe ye
    pad n m = genericTake (e' - n) (repeat 0) ++ m

normalize :: SDReal -> SDReal
normalize (SDReal (0:m) e) = normalize (SDReal m (pred e))
normalize (SDReal [] _) = SDReal [] 0
normalize x = x

sdrFromQ :: Rational -> SDReal
sdrFromQ r = normalize (goInt intPart (SDReal (goFrac fracPart) 0))
  where
    intPart :: Integer
    fracPart :: Rational
    (intPart, fracPart) = properFraction r

    goInt :: Integer -> SDReal -> SDReal
    goInt 0 x = x
    goInt n (SDReal m e) = goInt q (SDReal (fromInteger r : m) (succ e))
      where (q, r) = quotRem n sdBase

    goFrac :: Rational -> [Int64]
    goFrac 0 = []
    goFrac x = d : goFrac r
      where (d, r) = properFraction (x * fromInteger sdBase)
