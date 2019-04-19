
data ParetoOrdering
  = PEQ  -- Equivalent
  | PLT  -- Pareto less than
  | PGT  -- Pareto greater than
  | LLT  -- Lexically less than
  | LGT  -- Lexically greater than
  deriving (Eq, Show)

instance Monoid ParetoOrdering where
  mempty = PEQ
  mappend PEQ x   = x
  mappend PLT PLT = PLT
  mappend PLT PEQ = PLT
  mappend PLT _   = LLT
  mappend PGT PGT = PGT
  mappend PGT PEQ = PGT
  mappend PGT _   = LGT
  mappend LLT _   = LLT
  mappend LGT _   = LGT

-- Extract lexical ordering to pareto ordering
toLexOrd PEQ = EQ
toLexOrd PGT = GT
toLexOrd PLT = LT
toLexOrd LGT = GT
toLexOrd LLT = LT
compareLex a b = toLexOrd (paretoCompare a b)

-- Convert total ordering to pareto ordering.
fromTotalOrd EQ = PEQ
fromTotalOrd GT = PGT
fromTotalOrd LT = PLT
-- Useful as a default paretoCompare implementation.
compareTotal a b = fromTotalOrd (compare a b)

class ParetoOrd a where
  paretoCompare :: a -> a -> ParetoOrdering
  default paretoCompare :: Ord a => a -> a -> ParetoOrdering
  paretoCompare = compareTotal

instance ParetoOrd () where
  paretoCompare _ _ = PEQ

instance (ParetoOrd a, ParetoOrd b) => ParetoOrd (a, b) where
  paretoCompare (a1, a2) (b1, b2) = paretoCompare a1 b1 <> paretoCompare a2 b2

instance (ParetoOrd a, ParetoOrd b, ParetoOrd c)
         => ParetoOrd (a, b, c) where
  paretoCompare (a1, a2, a3) (b1, b2, b3)
    = paretoCompare (a1, (a2, a3)) (b1, (b2, b3))
