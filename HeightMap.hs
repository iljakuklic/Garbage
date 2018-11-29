
module HeightMap where

import Control.Monad
import Control.Applicative

inf = 1.0/0.0
ifte c t e = if c then t else e

type Angle = Float
type Height = Float
type Coords = (Float, Float)
type HeightMap = Coords -> Height

instance (Num a, Num b) => Num (a, b) where
  (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
  negate (x, y) = (negate x, negate y)
  (x1, y1) * (x2, y2) = (x1 * x2, y1 * y2)
  abs (x, y) = (abs x, abs y)
  fromInteger x = (fromInteger x, fromInteger x)
  signum (x, y) = (signum x, signum y)
instance (Fractional a, Fractional b) => Fractional (a, b) where
  recip (x, y) = (recip x, recip y)
  fromRational x = (fromRational x, fromRational x)

dot :: Coords -> Coords -> Float
dot a b = let (x, y) = a * b in x + y

-- flat surface with angle a
flat :: Angle -> HeightMap
flat a (x, _) = tan a * x

emptyMap :: HeightMap
emptyMap = const (-inf)

-- cylinder with radius 1 and height 1
cylinder c = if (c `dot` c) <= 1.0 then 1.0 else -inf

-- translate by (dx, dy)
tr :: Coords -> HeightMap -> HeightMap
tr d hmap c = hmap (c - d)

trH d hmap c = hmap c + d

-- rotate by angle a
rotate :: Angle -> HeightMap -> HeightMap
rotate a hmap c = let sa = sin a; ca = cos a in hmap ((ca, sa) `dot` c, (-sa, ca) `dot` c)

-- scale heightmap
scale :: Coords -> HeightMap -> HeightMap
scale s hmap c = hmap (c / s)

scaleH :: Height -> HeightMap -> HeightMap
scaleH s = fmap (*s)

higher, lower :: HeightMap -> HeightMap -> HeightMap
higher = liftA2 max  -- higher h1 h2 = \c -> max (h1 c) (h2 c)
lower  = liftA2 min

highest, lowest :: [HeightMap] -> HeightMap
highest = foldl higher emptyMap
lowest  = foldl lower (pure inf)

masked :: (Coords -> Bool) -> HeightMap -> HeightMap
masked mask hmap = liftA3 ifte mask hmap emptyMap
