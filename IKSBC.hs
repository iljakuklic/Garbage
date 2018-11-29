
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RankNTypes, GADTs #-}

import Bound
import Prelude hiding (elem)
import Prelude.Extras
import Data.Foldable
import Data.Traversable
import Data.Void

data Comb v
	= P v
	| I  -- \     v -> v
    | K  -- \ e   v -> e
    | S  -- \ f x v -> (f v) (x v)
    | B  -- \ f x v -> (f  ) (x v)
    | C  -- \ f x v -> (f v) (x  )
	deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Lam l v
	= V v
	| Lam l v :@ Lam l v
	| Abs l (Scope () (Lam l) v)
	deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Eq l => Eq1 (Lam l) where
instance Ord l => Ord1 (Lam l) where
instance Show l => Show1 (Lam l) where
instance Monad (Lam l) where
	return = V
	V x >>= f = f x
	(x :@ y) >>= f = (x >>= f) :@ (y >>= f)
	Abs l s >>= f = Abs l (s >>>= f)

--absElim :: Int -> Lam l (Either Int v) -> Lam Void (Either Int (Comb v))
absElim v (V (Left v')) | v == v'   = V (Right I)
                        | otherwise = V (Left v')
absElim v (V (Right x)) = V (Right (P x))
--absElim v term | Left v `notElem` term = 
