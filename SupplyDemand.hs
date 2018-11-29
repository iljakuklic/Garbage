import Data.List

equilibrium1 q sp dp (s:_) (d:_) | s > d = (q, sp, dp)
equilibrium1 q _ _ (s:spl) (d:dmd) = equilibrium1 (succ q) s d spl dmd
equilibrium1 q sp dp [] (d:_) = (q, max d sp, dp)
equilibrium1 q sp dp (s:_) [] = (q, sp, min s dp)
equilibrium1 q sp dp [] [] = (q, sp, dp)

equilibrium0 (s:spl) (d:dmd) | s <= d = Just (equilibrium1 1 s d spl dmd)
equilibrium0 _ _ = Nothing

equilibrium spl dmd = equilibrium0 (sort spl) (reverse $ sort dmd)

supply1 = [21, 23, 27, 28, 29, 32, 33, 33, 33, 35, 36, 45, 48, 49, 56, 61]
supply2 = [33]
demand1 = [85, 84, 71, 60, 55, 55, 54, 52, 43, 40, 38, 36, 36, 35, 32, 30]
demand2 = [43]
