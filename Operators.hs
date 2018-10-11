{-# LANGUAGE FlexibleInstances, FlexibleContexts, NoMonomorphismRestriction,
             DeriveFunctor #-}

import           Prelude hiding ((+), (-), (*), (/), (&&), (||), Monoid)
import qualified Prelude as P

class Semigroup a where
  (<>) :: a -> a -> a

class Semigroup a => Monoid a where
  neutral :: a

class Monoid a => Group a where
  inverse :: a -> a

newtype Add a = Add { getAdd :: a } deriving (Functor)
newtype Mul a = Mul { getMul :: a } deriving (Functor)
newtype And a = And { getAnd :: a } deriving (Functor)

infixl 6 +, -
infixl 7 *, /

(+) :: Semigroup (Add a) => a -> a -> a
a + b = getAdd (Add a <> Add b)
(-) :: Group (Add a) => a -> a -> a
a - b = getAdd (Add a <> inverse (Add b))
(*) :: Semigroup (Mul a) => a -> a -> a
a * b = getMul (Mul a <> Mul b)
(/) :: Group (Mul a) => a -> a -> a
a / b = getMul (Mul a <> inverse (Mul b))

instance Semigroup (Add Integer) where
  Add a <> Add b = Add (a P.+ b)
instance Monoid (Add Integer) where
  neutral = Add 0
instance Group (Add Integer) where
  inverse = fmap negate
