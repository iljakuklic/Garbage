
-- A bunch of simple interval propagators

data Interval = I Float Float deriving (Show)

inf = read "Infinity"
ninf = -inf
atmost x = I ninf x
atleast x = I x inf

instance Semigroup Interval where
  I l1 h1 <> I l2 h2 = I (max l1 l2) (min h1 h2)

instance Monoid Interval where
  mempty = I ninf inf

lteProp (x@(I xl xh), y@(I yl yh))
  = (x <> atmost yh, y <> atleast xl)

maxProp (r@(I rl rh), x@(I xl xh), y@(I yl yh))
  = (I (max xl rl) (max xh yh),
     I (if yh < max rl xl then max rl xl else xl) (min xh rh),
     I (if xh < max rl yl then max rl yl else yl) (min yh rh))

increasing f finv rx@(I rl rh, I xl xh)
  = (I (f xl) (f xh), I (finv rl) (finv rh)) <> rx
decreasing f finv rx@(I rl rh, I xl xh)
  = (I (f xh) (f xl), I (finv rh) (finv rl)) <> rx

scaled k | k > 0.0 = increasing (*k) (/k)
scaled k | k < 0.0 = decreasing (*k) (/k)
