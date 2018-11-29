
module Series where

import Control.Applicative
import Data.Complex

data Series a = a :+: Series a deriving Eq

series n as x = seriesAcc 0 (n::Int) 1 as
	where
		seriesAcc y 0 _ _ = y
		seriesAcc y n xn (a :+: as) = seriesAcc (y+a*xn) (n-1) (xn * x) as

sList (x :+: xs) = x : sList xs

natSeries n = n :+: natSeries (n + 1)

instance Show a => Show (Series a) where show = showSeries "x" 8

showSeries v n = (++ "...") . concatMap ((++ " + ") . showElem) . zip [0..] . take n . sList
	where
		showElem (0, a) = showP a
		showElem (1, a) = showP a ++ v
		showElem (n, a) = showP a ++ v ++ "^" ++ show n
		showP x = "(" ++ show x ++ ")"

printSeries v n = putStrLn . showSeries v n

infixr 6 :+:

x :: Num a => Series a
x = 0 :+: 1 :+: pure 0

instance Functor Series where
	fmap f (x :+: xs) = f x :+: fmap f xs

instance Applicative Series where
	pure x = let sx = x :+: sx in sx
	(f :+: fs) <*> (x :+: xs) = f x :+: (fs <*> xs)

instance Num a => Num (Series a) where
	fromInteger n = fromInteger n :+: pure 0
	negate = fmap negate
	x + y = (+) <$> x <*> y
	(x:+:xs) * yS@(y:+:ys) = x * y :+: (fmap (x*) ys + xs * yS)
	abs    = error "abs not implemented for Series"
	signum = error "signum not implemented for Series"

instance (Fractional a, Eq a) => Fractional (Series a) where
	fromRational x = fromRational x :+: pure 0
	(0:+:xs) / (0:+:ys) = xs / ys
	(x:+:xs) / yS@(y:+:ys) = let q = x / y in q :+: (xs - fmap (q*) ys) / yS

integrate c f = c :+: ((/) <$> f <*> natSeries 1)
diff (_:+:f1) = (*) <$> natSeries 1 <*> f1
diffs = iterate diff

-- TKSL syntax (almost)
infixl 0 &
(&) = flip integrate

exp_x = exp_x  & 1
sin_x =  cos_x & 0
cos_x = -sin_x & 1
cexp  = -cexp  & 0:+1 & 1:+0

inv_x_1 = -(inv_x_1 * inv_x_1) & 1

fourier f = integrate 0 (f * cexp)

-- TODO tan, atan, pi, log, sqrt, sigmoid
-- abs, signum, sqwave, triwave, floor??
-- conversion to digits
-- fourier transform?, laplace transform
-- Pipes interface

