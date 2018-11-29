
infixl 3 <>

interface Mon a where
  eps : a
  (<>) : a -> a -> a
  mon_left_neutral : (x : a) -> x <> eps = x
  mon_right_neutral : (x : a) -> eps <> x = x
  mon_assoc : (x : a) -> (y : a) -> (z : a) -> x <> (y <> z) = (x <> y) <> z

[andMon] Mon Bool where
  eps = True
  x <> y = x && Delay y

