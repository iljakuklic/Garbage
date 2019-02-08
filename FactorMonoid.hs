{-# LANGUAGE NoMonomorphismRestriction, DefaultSignatures,
             PatternSynonyms, ViewPatterns,
             TypeFamilies, GADTs #-}

-- Inspired by monoid-subclasses and mono-traversable

import Data.Monoid
import Data.Maybe
import Data.List (genericReplicate)
import qualified Data.Map as M

-- Monoid with decidable mempty testing
-- Laws:
--   isNull mempty = True
--   isNull x = false    -- wher x is not mempty
class Monoid m => NullMonoid m where
  -- check if value is mempty
  isNull :: m -> Bool
  -- fall back on Eq by default
  default isNull :: Eq m => m -> Bool
  isNull = (mempty ==)

-- a bunch of instences
instance NullMonoid Any
instance NullMonoid All
instance NullMonoid Ordering
instance (Eq a, Num a) => NullMonoid (Sum a)
instance (Eq a, Num a) => NullMonoid (Product a)
instance NullMonoid [a] where isNull = null
instance Ord k => NullMonoid (M.Map k v) where isNull = M.null
instance NullMonoid (Last a) where isNull = isNothing . getLast
instance NullMonoid (First a) where isNull = isNothing . getFirst
instance (NullMonoid m) => NullMonoid (Dual m) where isNull = isNull . getDual
-- instance (Finite a, NullMonoid b) => NullMonoid (a -> b) where ......

-- Match mempty using the Null view pattern synonym
pattern Null :: NullMonoid m => m
pattern Null <- (isNull -> True)
  where Null = mempty

-- ***** CLASS

-- Monoid that can be broken down into further indivisible factors.
-- For Product, these are prime factors, for list, these are its elements etc.
--
-- Laws:
--   mconcat . map singleton . factors = id
--   factors (singleton x) = [x]
--   (more...)
class NullMonoid m => FactorMonoid m where
  -- type of a prime factor fir this monoid
  type Factor m :: *

  -- Convert a prime factor into monoid representation,
  -- creating a monoid that is indivisible.
  singleton :: Factor m -> m

  -- Extract prime factors from the monoid
  factors :: m -> [Factor m]

-- ***** INSTANCES

-- Factorize a list into its elements
instance FactorMonoid [a] where
  type Factor [a] = a
  singleton = pure
  factors = id

-- Break a map into its elements
instance Ord k => FactorMonoid (M.Map k v) where
  type Factor (M.Map k v) = (k, v)
  singleton = uncurry M.singleton
  factors = M.toList

-- Factorize the product monoid
instance Integral a => FactorMonoid (Product a) where
  type Factor (Product a) = Prime a
  singleton = Product . getPrime
  factors (Product x) = primeFactors x

-- Factorize the sum monoid. Prime factors are 1s.
instance Integral a => FactorMonoid (Sum a) where
  type Factor (Sum a) = ()
  singleton () = 1
  factors = flip genericReplicate () . getSum

-- ***** OPERATIONS

-- ***** HELPERS

check :: (a -> Bool) -> a -> Maybe a
check p x = if p x then Just x else Nothing

-- Wrapper around numbers that only contains prime numbers
newtype Prime a = Prime { getPrime :: a } deriving (Eq, Ord, Show)
toPrime :: Integral a => a -> Maybe (Prime a)
toPrime = fmap Prime . check isPrime

-- Super naive prime factorization
isPrime x = and [ x `mod` n /= 0 | n <- [2..pred x] ]
primeFactorsP 0 p = error "Cannot factorize 0"
primeFactorsP 1 p = []
primeFactorsP x p = maybe next onPrime (toPrime p) where
  next = primeFactorsP x (succ p)
  (x', m) = x `divMod` p
  onPrime p' = if m == 0 then p' : primeFactorsP x' p else next
primeFactors x = primeFactorsP x 2
