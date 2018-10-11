{-# LANGUAGE
      RankNTypes, ExistentialQuantification, InstanceSigs,
      NoMonomorphismRestriction, LambdaCase #-}

import qualified Prelude as P
import           Prelude hiding (sum, product, length, all, any, and, or,
                                 filter)
import           Control.Applicative
import qualified Data.Map as M

data Query e a = forall m . Query {
    qPre :: e -> m,
    qEps :: m,
    qApp :: m -> m -> m,
    qOut :: m -> a
  }

query :: Foldable t => Query e a -> t e -> a
query (Query pre eps app out)
  = out . foldr (\x acc -> app (pre x) acc) eps

simple eps app = Query id eps app id

collect = Query pure [] (++) id
sum = simple 0 (+)
product = simple 1 (*)
and = simple True (&&)
or = simple False (||)
count = emap (const 1) sum
all f = emap f and
any f = emap f or
first = Query Just Nothing (maybe id (\x _ -> Just x)) id
last = dual first
dual (Query pre eps app out) = Query pre eps (flip app) out

instance Functor (Query e) where
  fmap :: (a -> b) -> (Query e a -> Query e b)
  fmap f (Query pre eps app out) = Query pre eps app (f . out)

-- contravariant component of profunctor
emap :: (e -> d) -> (Query d a -> Query e a)
emap f (Query pre eps app out) = Query (pre . f) eps app out

filter :: (e -> Bool) -> Query e a -> Query e a
filter p (Query pre eps app out) = Query pre' eps app out
  where pre' x = if p x then pre x else eps

group :: Ord k => (e -> k) -> Query e a -> Query e (M.Map k a)
group g (Query pre eps app out)
  = Query {
      qPre = \x -> M.singleton (g x) (pre x),
      qEps = M.empty,
      qApp = M.unionWith app,
      qOut = fmap out
    }

instance Applicative (Query e) where
  pure :: a -> Query e a
  pure x = Query (const ()) () mappend (const x)
  (<*>) :: Query e (a -> b) -> Query e a -> Query e b
  Query pref epsf appf outq <*> Query prex epsx appx outx
    = Query {
        qPre = \e -> (pref e, prex e),
        qEps = (epsf, epsx),
        qApp = \(af, ax) (bf, bx) -> (appf af bf, appx ax bx),
        qOut = \(rf, rx) -> outq rf (outx rx)
      }

instance Num a => Num (Query e a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Query e a) where
  recip = fmap recip
  fromRational = pure . fromRational
