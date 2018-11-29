{-# LANGUAGE NoMonomorphismRestriction #-}

module CHF where

import Data.Ratio
import Data.Monoid

-- Homographic function
--
--                 a + b*x
-- HF a b c d  =   -------
--                 c + d*x
data HF a = HF a a a a deriving (Show, Eq)

-- smart constructor (normalizes by gcd)
hf a b c d = let g = gcd a b `gcd` gcd c d
		in HF (a `div` g) (b `div` g) (c `div` g) (d `div` g)

evalLF (HF a b c d) = \x -> (a + b * x) / (c + d * x)
hfNat n = hfFrac n 1
hfFrac n d = hf n n d d
hfLin a b = HF a b 1 0
hfInv = HF 1 0 0 1
hfNeg = HF 0 (negate 1) 1 0
hfMul k = HF 0 k 1 0
hfSub n = HF (negate n) 1 1 0
hf_1 (HF a b c d) = HF (-a) c b (-d)

instance Integral a => Monoid (HF a) where
	mempty = HF 0 1 1 0
	mappend (HF a1 b1 c1 d1) (HF a2 b2 c2 d2) =
		hf (b1*a2 + a1*c2) (b1*b2 + a1*d2) (d1*a2 + c1*c2) (d1*b2 + c1*d2)

newtype CHF a = CHF [HF a] deriving Show
unCHF (CHF x) = x

instance Num a => Monoid (CHF a) where
	mempty = CHF []
	(CHF fs1) `mappend` (CHF fs2) = CHF (fs1 ++ fs2)

-- strict application
chfAp hf (CHF []) = CHF [hf]
chfAp hf (CHF (a:as)) = CHF ((hf <> a) : as)
chfStep (CHF (a:as)) = chfAp a (CHF as)

instance Integral a => Num (CHF a) where
	(+) = undefined
	(*) = undefined
	abs = undefined
	signum = undefined
	negate = chfAp hfNeg
	fromInteger = CHF . return . hfNat . fromInteger

instance Integral a => Fractional (CHF a) where
	(/) = undefined
	recip = chfAp hfInv
	fromRational = fromReal . fromRational

fromReal x = let n = numerator x; d = denominator x in CHF [hf n n d d]
fromGCF xi xf  = CHF (HF xi 1 1 0 : [ hf a 0 b 1 | (a, b) <- xf ])
fromCF (xi:xf) = fromGCF xi $ map (\b -> (1, b)) xf
fromPS as x = CHF [ hf a x 1 0 | a <- as ]

chfTerms = scanl (<>) mempty . unCHF

hasIntPart (HF a b c d) = c /= 0 && d /= 0 && a `div` c == b `div` d
foldInt x@(a:as) | hasIntPart a = x
foldInt (a:b:cs) = foldInt $ (a <> b) : cs
foldInt x = x

scanInts f = scanInts' f . unCHF
scanInts' f chf = case foldInt chf of
	[] -> []
	[HF 0 _ 0 _] -> []
	hf@(HF a b c d) : hfs ->
		let x = a `div` c in x : scanInts' f (f x hf : hfs)

toDigits base = scanInts (\x hf -> hfMul base <> hfSub x <> hf)

showDigits = digitStr' . toDigits 10
  where
    digitStr' [] = "0"
    digitStr' (a:as) = show a ++ '.' : concatMap show as

toCF = scanInts (\x hf -> hfInv <> hfSub x <> hf)

phi = fromCF (repeat 1)
pi' = fromGCF 0 (zip (4 : [ x*x | x <- [1..]]) [1,3..])

{-

toCF = toCF' . unCHF
toCF' [] = []
toCF' (HF a b c d : hfs) | c /= 0 && d /= 0 && a `div` c == b `div` d =
	let x = a `div` c in x : toCF (chfStep . CHF $ ((hfSub x <> hfInv) : hfs))
toCF' hfs = toCF (chfStep (CHF hfs))

mixed x = let xi = floor x in (xi, x - fromIntegral xi)

cf x = let (xi, xf) = mixed x in xi : cff xf
cff 0 = []
cff x = let (xi, xf) = mixed (recip x) in xi : cff xf

lbd (CF xi xf) = let (yi, d) = foldr lbdStep (0, 1) (map fromIntegral xf) in (yi + xi, d)
lbdStep xf (x, d) = (recip (x + xf), 0)

convFracs (CF xi xfs) =  drop 2 cfs
	where cfs = (0, 1) : (1, 0) :
		[ (a * h1 + h2, a * k1 + k2) |
		(a, (h1, k1), (h2, k2)) <- zip3 (xi:xfs) (drop 1 cfs) cfs ]

convs = map (\(a, b) -> fromIntegral a / fromIntegral b) . convFracs

general gs = zipWith (*) cs (map snd gs)
	where cs = 1 : [ recip (a * c) | (a, c) <- zip (map fst gs) cs ]

piGeneral = zip (4 : [ x*x | x <- [1..]]) [1,3..]
-}
