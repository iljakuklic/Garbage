{-# LANGUAGE NoMonomorphismRestriction, DefaultSignatures,
             PatternSynonyms, ViewPatterns,
             TypeFamilies, GADTs, FlexibleContexts,
             GeneralizedNewtypeDeriving #-}

-- Inspired by monoid-subclasses and mono-traversable

import Data.Monoid
import Data.Maybe
import Data.Void
import Data.List (genericReplicate)
import qualified Data.Map as M
import qualified Data.List as L

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
instance NullMonoid ()
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
instance (NullMonoid m, NullMonoid n) => NullMonoid (m, n) where
  isNull (x, y) = isNull x && isNull y
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
  -- type of a prime factor for this monoid
  type Factor m :: *

  -- Convert a prime factor into monoid representation,
  -- creating a monoid that is indivisible.
  singleton :: Factor m -> m

  -- Extract prime factors from the monoid
  factors :: m -> [Factor m]
  factors = L.unfoldr factorL

  -- Separate the leftmost factor
  factorL :: m -> Maybe (Factor m, m)
  factorL a = case factors a of
    (x:xs) -> Just (x, mconcat (map singleton xs))
    [] -> Nothing

  -- Separate the leftmost factor
  factorR :: m -> Maybe (m, Factor m)
  factorR a = case reverse (factors a) of
    (x:xs) -> Just (mconcat (map singleton (reverse xs)), x)
    [] -> Nothing

-- ***** INSTANCES

-- Trivial unit instance
instance FactorMonoid () where
  type Factor () = Void
  singleton = absurd
  factors () = []

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
  factorL = M.minViewWithKey
  factorR = fmap (\(a, b) -> (b, a)) . M.maxViewWithKey

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

-- First monoid
instance FactorMonoid (First a) where
  type Factor (First a) = a
  singleton = First . Just
  factors = maybeToList . getFirst

-- Any monoid
instance FactorMonoid Any where
  type Factor Any = ()
  singleton () = Any True
  factors (Any True) = [()]
  factors (Any False) = []

-- Dual monoid reverses the operation
instance FactorMonoid m => FactorMonoid (Dual m) where
  type Factor (Dual m) = Factor m
  singleton = Dual . singleton
  factors = reverse . factors . getDual
  factorL = fmap swap . factorR
  factorR = fmap swap . factorL

-- Factorize a pair, componentwise
instance (FactorMonoid m, FactorMonoid n) => FactorMonoid (m, n) where
  type Factor (m, n) = Either (Factor m) (Factor n)
  singleton = either (\x -> (singleton x, mempty)) (\x -> (mempty, singleton x))
  factors (a, b) = fmap Left (factors a) ++ fmap Right (factors b)

-- ***** OPERATIONS

-- Convenience patterns for constructiong & deconstructing monoids

infixr 5 :<:
pattern (:<:) :: FactorMonoid m => Factor m -> m -> m
pattern x :<: xs <- (factorL -> Just (x, xs))
  where x :<: xs = singleton x <> xs

infixl 5 :>:
pattern (:>:) :: FactorMonoid m => m -> Factor m -> m
pattern xs :>: x <- (factorR -> Just (xs, x))
  where xs :>: x = xs <> singleton x

-- Operations, suffixed 'F' to disambiguate from standard functions

foldMapF :: (FactorMonoid m, Monoid n) => (Factor m -> n) -> m -> n
foldMapF f = mconcat . map f . factors

monoFoldMapF :: FactorMonoid m => (Factor m -> m) -> m -> m
monoFoldMapF = foldMapF

mapF :: (FactorMonoid m, FactorMonoid n) => (Factor m -> Factor n) -> m -> n
mapF f = foldMapF (singleton . f)

monoMapF :: FactorMonoid m => (Factor m -> Factor m) -> m -> m
monoMapF = mapF

withFactors :: FactorMonoid m => ([Factor m] -> [Factor m]) -> m -> m
withFactors f = mconcat . map singleton . f . factors

headF = fmap fst . factorL
tailF = fmap snd . factorL
initF = fmap fst . factorR
lastF = fmap snd . factorR

reverseF :: FactorMonoid m => m -> m
reverseF = mapF id . Dual

takeF n = withFactors (take n)
dropF n = withFactors (drop n)
takeWhileF p = withFactors (takeWhile p)
dropWhileF p = withFactors (dropWhile p)

toListF = factors
lengthF = length . factors
sumF = getSum . foldMapF Sum
productF = getProduct . foldMapF Product
filterF p = monoFoldMapF (checkF p)
anyF p = getAny . foldMapF (Any . p)
allF p = getAll . foldMapF (All . p)
noneF p = not . anyF p
elemF x = anyF (== x)
findF p = getFirst . foldMapF (First . check p)
concatF = foldMapF id
checkF p x = if p x then singleton x else mempty

traverseF :: (Monoid a, FactorMonoid m, Applicative f)
          => (Factor m -> f a) -> m -> f a
traverseF f = runAct . foldMapF (Act . f)
forF = flip traverseF

isNullDefault = isNothing . factorL

-- ***** HELPERS

check :: (a -> Bool) -> a -> Maybe a
check p x = if p x then Just x else Nothing
checkM :: Monoid m => (m -> Bool) -> m -> m
checkM p x = if p x then x else mempty

swap (x, y) = (y, x)

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

-- Effectful actions as monoids
newtype Act f a = Act { runAct :: f a } deriving (Functor, Applicative)
instance (Applicative f, Monoid a) => Monoid (Act f a) where
  mempty = pure mempty
  mappend (Act a) (Act b) = Act (mappend <$> a <*> b)
