{-# LANGUAGE DefaultSignatures, NoMonomorphismRestriction, MultiParamTypeClasses,
             FunctionalDependencies, GADTs, MonadComprehensions, TypeFamilies #-}

import Prelude hiding ((>>))
import Control.Applicative
import Data.Foldable
import Data.Monoid

(>>) = (*>)

feasibility pred x y = (pred <$> x <*> y) >>= \p -> if p then y else empty

infixl 4 ==., <=., <.

class OrdC a where
  (==.) :: a -> a -> a
  (<=.) :: a -> a -> a
  (<.) :: a -> a -> a

  default (==.) :: (a ~ f b, Monad f, Alternative f, Eq b) => f b -> f b -> f b
  (==.) = feasibility (==)
  default (<=.) :: (a ~ f b, Monad f, Alternative f, Ord b) => f b -> f b -> f b
  (<=.) = feasibility (<=)
  default (<.) :: (a ~ f b, Monad f, Alternative f, Ord b) => f b -> f b -> f b
  (<.) = feasibility (<)

instance Ord b => OrdC (Maybe b)

subjectTo :: Applicative f => f a -> [f a] -> f a
subjectTo obj ctrs = obj <* sequenceA_ ctrs

-- One-variable unconstrained models:
quad1 x = x^2 - 2 * x

-- One-variable constrained models:
lin1 x = x + 3 `subjectTo` [3 <=. x, x <=. 12]
quad2 x = quad1 x `subjectTo` [3 <=. x, x <=. 12]

-- Two-variable diet problem
diet :: (OrdC (f a), Num (f a), Applicative f) => (a, a) -> f a
diet (be, bu) = (3 * beer + 5 * burger)
             <* (0 <=. beer <=. 3) -- <* 0.5 <=. beer <=. 2.5 -- (requires fractional)
             <* (0 <=. burger <=. 4)
             <* (220 <=. 10 * beer + 200 * burger <=. 650)
  where beer = pure be; burger = pure bu


data ExprNum = EAdd ExprNum ExprNum | ENeg ExprNum | EMul ExprNum ExprNum
             | EAbs ExprNum | ESignum ExprNum | EConst Integer | EVar String
             deriving (Show)

instance Num ExprNum where
  (+) = EAdd
  (*) = EMul
  negate = ENeg
  abs = EAbs
  signum = ESignum
  fromInteger = EConst

instance Num a => Num (Maybe a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

data ExprOrd = EEql ExprNum ExprNum | ELess ExprNum ExprNum | ELessEq ExprNum ExprNum
             deriving (Show)

data Model a = Model { objective :: a, constraints :: [ExprOrd] } deriving Show

mkExprOrd p (Model oA cA) (Model oB cB) = Model oB (p oA oB : cA <> cB)

instance Num a => Num (Model a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (a ~ ExprNum) => OrdC (Model a) where
  (==.) = mkExprOrd EEql
  (<=.) = mkExprOrd ELessEq
  (<. ) = mkExprOrd ELess

instance Functor Model where
  fmap f (Model x p) = Model (f x) p

instance Applicative Model where
  pure x = Model x []
  Model f p <*> Model x q = Model (f x) (p <> q)

class DecVar a where
  type DecResult a :: *
  type DecResult a = ()
  mkDecVar :: String -> a

instance DecVar ExprNum where
  type DecResult ExprNum = Float
  mkDecVar = EVar

instance (a ~ ExprNum) => DecVar (Model a) where
  type DecResult (Model a) = Maybe (DecResult a)
  mkDecVar = pure . mkDecVar

instance (DecVar a, DecVar b) => DecVar (a, b) where
  type DecResult (a, b) = (DecResult a, DecResult b)
  mkDecVar s = (mkDecVar (s <> "_0"), mkDecVar (s <> "_1"))

instance (DecVar a, DecVar b, DecVar c) => DecVar (a, b, c) where
  type DecResult (a, b, c) = (DecResult a, DecResult b, DecResult c)
  mkDecVar s = (mkDecVar (s <> "_1"), mkDecVar (s <> "_1"), mkDecVar (s <> "_2"))

toExpr f = f (mkDecVar "var")

minimize :: (DecVar a) => (a -> Model ExprNum) -> DecResult a
minimize = undefined
