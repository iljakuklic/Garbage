{-# LANGUAGE
       NoMonomorphismRestriction, GADTs, PolyKinds, TypeFamilies,
       MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
       StandaloneDeriving, RankNTypes
#-}

import Prelude hiding (id, ($), (.), fst, snd, curry, uncurry)
import qualified Prelude as P
import Control.Category
import Control.Monad.Fix(fix)
import Data.Functor.Identity
import Data.Functor.Const

class Category cat => Terminal cat where
  kill :: cat a ()

instance Terminal (->) where
  kill = P.const ()

infix 7 ><

class Terminal cat => Cartesian cat where
  fst :: cat (a, b) a
  snd :: cat (a, b) b
  (><) :: cat a b -> cat a c -> cat a (b, c)

instance Cartesian (->) where
  fst = P.fst
  snd = P.snd
  (f >< g) x = (f x, g x)

class Cartesian cat => Closed cat where
  curry :: cat (a, b) c -> cat a (b -> c)
  uncurry :: cat a (b -> c) -> cat (a, b) c
  apply :: cat (a -> b, a) b

instance Closed (->) where
  curry = P.curry
  uncurry = P.uncurry
  apply (f, x) = f x

assoc :: Cartesian cat => cat ((a, b), c) (a, (b, c))
assoc = fst . fst >< (snd . fst >< snd)

unassoc :: Cartesian cat => cat (a, (b, c)) ((a, b), c)
unassoc = (fst >< fst . snd) >< snd . snd

swap :: Cartesian cat => cat (a, b) (b, a)
swap = snd >< fst

cross :: Closed cat => cat (a -> b, a -> c) (a -> (b, c))
cross = curry (apply . (fst . fst >< snd) >< apply . (snd . fst >< snd))

comp :: Closed cat => cat (b -> c, a -> b) (a -> c)
comp = curry (apply . (fst >< apply . snd) . assoc)

ccurry :: Closed cat => cat ((a, b) -> c) (a -> b -> c)
ccurry = curry (curry (apply . assoc))

cuncurry :: Closed cat => cat (a -> b -> c) ((a, b) -> c)
cuncurry = curry (apply . (apply . (fst >< fst . snd) >< snd . snd))

cfst :: Closed cat => cat () ((a, b) -> a)
cfst = curry (fst . snd)

data FreeCCC a b where
  Id :: FreeCCC a a
  Kill :: FreeCCC a ()
  Fst :: FreeCCC a (b, c) -> FreeCCC a b
  Snd :: FreeCCC a (b, c) -> FreeCCC a c
  Cross :: FreeCCC a b -> FreeCCC a c -> FreeCCC x a -> FreeCCC x (b, c)
  Curry :: FreeCCC (a, b) c -> FreeCCC x a -> FreeCCC x (b -> c)
  Uncurry :: FreeCCC a (b -> c) -> FreeCCC x (a, b) -> FreeCCC x c
  Apply :: FreeCCC x (a -> b, a) -> FreeCCC x b

deriving instance (Show (FreeCCC a b))

instance Category FreeCCC where
  id = Id
  Id . f = f
  Kill . g = Kill
  Fst f . g = Fst (f . g)
  Snd f . g = Snd (f . g)
  (Cross f g h) . h' = Cross f g (h . h')
  Curry f g . h = Curry f (g . h)
  Uncurry f g . h = Uncurry f (g . h)
  Apply f . h = Apply (f . h)
instance Terminal FreeCCC where
  kill = Kill
instance Cartesian FreeCCC where
  fst = Fst Id
  snd = Snd Id
  f >< g = Cross f g Id
instance Closed FreeCCC where
  curry f = Curry f Id
  uncurry f = Uncurry f Id
  apply = Apply Id

freeCCC :: Closed cat => FreeCCC a b -> cat a b
freeCCC Id = id
freeCCC Kill = kill
freeCCC (Fst f) = fst . freeCCC f
freeCCC (Snd f) = snd . freeCCC f
freeCCC (Cross f g h) = (freeCCC f >< freeCCC g) . freeCCC h
freeCCC (Curry f g) = curry (freeCCC f) . freeCCC g
freeCCC (Uncurry f g) = uncurry (freeCCC f) . freeCCC g
freeCCC (Apply g) = apply . freeCCC g

optimize :: FreeCCC a b -> FreeCCC a b
optimize Id = optstep Id
optimize Kill = optstep Kill
optimize (Fst f) = optstep (Fst (optimize f))
optimize (Snd f) = optstep (Snd (optimize f))
optimize (Cross f g h) = optstep (Cross (optimize f) (optimize g) (optimize h))
optimize (Curry f g) = optstep (Curry (optimize f) (optimize g))
optimize (Uncurry f g) = optstep (Uncurry (optimize f) (optimize g))
optimize (Apply f) = optstep (Apply (optimize f))

f <.> g = optstep (f . g)

optstep :: FreeCCC a b -> FreeCCC a b
optstep (Fst (Cross f g h)) = f <.> h
optstep (Snd (Cross f g h)) = g <.> h
optstep (Cross (Fst Id) (Snd Id) h) = optstep h
optstep (Uncurry (Curry f Id) g) = f <.> g
optstep (Curry (Uncurry f Id) g) = f <.> g
optstep (Apply (Cross (Curry f q) g h)) = f <.> optstep (Cross q g h)
optstep f = f

data LC v a where
  Var :: v a -> LC v a
  Lam :: (v a -> LC v b) -> LC v (a -> b)
  App :: LC v (a -> b) -> LC v a -> LC v b

-- Closed lambda calculus term
type CLC a = forall v . LC v a

infixr 0 $$
($$) = App
lam f = Lam (f . Var)

evalLC :: CLC a -> a
evalLC = run
  where
    run :: LC Identity a -> a
    run (Var x) = runIdentity x
    run (Lam f) = run . f . Identity
    run (App f x) = run f (run x)

showLC :: CLC a -> String
showLC = run 0
  where
    run :: Int -> LC (Const Int) a -> String
    run _ (Var x) = "x" ++ show (getConst x)
    run n (Lam f) = "\\x" ++ show n ++ ". " ++ run (succ n) (f (Const n))
    run n (App f x) = "(" ++ run n f ++ " " ++ run n x ++ ")"

lccomp = lam (\f -> lam (\g -> lam (\x -> f $$ g $$ x)))

{-
lc2ccc :: Closed cat => LC (cat a) b -> cat a b
lc2ccc (Var x) = x
lc2ccc (Lam f) = curry (lc2ccc (f (_bla . kill)) . fst)
lc2ccc (App f x) = apply . (lc2ccc f >< lc2ccc x)
-}

lc2ccc :: Closed cat => LC (cat ()) b -> cat () b
lc2ccc (Var x) = x
lc2ccc (Lam f) = _lam
lc2ccc (App f x) = apply . (lc2ccc f >< lc2ccc x)



