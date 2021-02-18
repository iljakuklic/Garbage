{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}

-- An attempt at embedding of disciplined convex programming in Haskell.

class Convex (t :: * -> *)
class Concave (t :: * -> *)

type Affine t = (Convex t, Concave t)

-- Constant
newtype C t = C t deriving (Show)
instance Convex C
instance Concave C

-- Maximum
data Max f g t = Max (f t) (g t) deriving (Show)
instance (Convex f, Convex g) => Convex (Max f g)

-- Negation
data Neg f t = Neg (f t) deriving (Show)
instance Convex f => Concave (Neg f)
instance Concave f => Convex (Neg f)

-- Addition
data Add f g t = Add (f t) (g t) deriving (Show)
instance (Convex f, Convex g) => Convex (Add f g)
instance (Concave f, Concave g) => Concave (Add f g)

-- Scaling by a non-negative constant
data Scale f t = Scale t (f t) deriving (Show)
instance Convex f => Convex (Scale f)
instance Concave f => Concave (Scale f)


isConvex :: Convex f => f t -> f t
isConvex = id

isConcave :: Concave f => f t -> f t
isConcave = id

isAffine :: Affine f => f t -> f t
isAffine = id

absolute :: Affine f => f t -> Max f (Neg f) t
absolute x = Max x (Neg x)

neg = Neg
add = Add
sub x y = add x (neg y)

class Eval f where
  eval :: f a -> a



