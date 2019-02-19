
{-# LANGUAGE GADTs, NoMonomorphismRestriction, RankNTypes #-}

import qualified Control.Monad.State as S

data Mon f a where
  Pure :: a -> Mon f a
  Bind :: f a -> (a -> Mon f b) -> Mon f b

mon = flip Bind Pure

instance Monad (Mon f) where
  return = Pure
  Pure x >>= fb = fb x
  Bind x fa >>= fb = Bind x (\x' -> fa x' >>= fb)
instance Applicative (Mon f) where
  pure = Pure
  Pure f <*> x = fmap f x
  Bind fa f <*> x = Bind fa (\x' -> f x' <*> x)
instance Functor (Mon f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Bind fa x) = Bind fa (fmap f . x)

run :: (forall a . a -> g a)
    -> (forall a b . f a -> (a -> g b) -> g b)
    -> Mon f a -> g a
run pur alg = run'
  where
    run' (Pure x) = pur x
    run' (Bind fa k) = alg fa (run' . k)

data Ste s a where
  Get :: Ste s s
  Set :: s -> Ste s ()

get :: Mon (Ste s) s
get = mon Get
set :: s -> Mon (Ste s) ()
set = mon . Set

gets f = fmap f get

modify :: (a -> a) -> Mon (Ste a) ()
modify f = do
  s <- get
  set (f s)

tick :: Mon (Ste Int) ()
tick = modify succ

runSte :: Mon (Ste s) a -> s -> (s, a)
runSte (Pure x) s = (s, x)
runSte (Bind Get k) s = runSte (k s) s
runSte (Bind (Set s') k) s = runSte (k ()) s'

-- Freer monad final encoding
newtype Freer f a = Freer {
  runFreer :: forall m. Monad m => (forall b. f b -> m b) -> m a }

instance Functor (Freer f) where
  fmap f gm = Freer (\x -> fmap f (runFreer gm x))

instance Applicative (Freer f) where
  pure x = Freer (const (pure x))
  ff <*> xx = Freer (\k -> (runFreer ff k) <*> (runFreer xx k))

instance Monad (Freer f) where
  return = pure
  ma >>= f = Freer (\k -> runFreer ma k >>= \x -> runFreer (f x) k)

interpState :: Ste s a -> S.State s a
interpState Get = S.get
interpState (Set x) = S.put x

runF :: Monad m => (forall b. f b -> m b) -> Freer f a -> m a
runF nt act = runFreer act nt

runSteF :: Freer (Ste s) a -> S.State s a
runSteF = runF interpState
