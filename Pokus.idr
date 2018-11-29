
data PBase = M | S | Kg

PUnit : Type
PUnit = PBase -> Int

m : PUnit
m M = 1
m _ = 0

m2 : PUnit
m2 M = 2
m2 _ = 0

s : PUnit
s S = 1
s _ = 0

kg : PUnit
kg Kg = 1
kg _ = 0

kilo : PUnit
kilo Kg = 1
kilo S = 0
kilo M = 0

scalar : PUnit
scalar _ = 0

data Quantity : PUnit -> Type where
  Q : Double -> Quantity u

mk : Double -> (u : PUnit) -> Quantity u
mk x _ = Q x

add : Quantity u -> Quantity u -> Quantity u
add (Q x) (Q y) = Q (x + y)

mult : Quantity u1 -> Quantity u2 -> Quantity (\x => u1 x + u2 x)
mult (Q x) (Q y) = Q (x * y)

ap : (f : a -> b) -> (x = y) -> (f x = f y)
ap f Refl = Refl

