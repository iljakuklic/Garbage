
newtype Log = Log { getLog :: Float } deriving (Eq, Ord, Show)

instance Num Log where
  fromInteger = Log . log . fromInteger
  signum _ = Log 0
  abs = id
  negate = error "Cannot negate"
  Log x + Log y = Log (log (exp x + exp y))
  Log x * Log y = Log (x + y)

-- d/dx log (1 + exp x) = exp x / (1 + exp x)
