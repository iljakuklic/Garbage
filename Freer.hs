
{-# LANGUAGE GADTs, NoMonomorphismRestriction, RankNTypes #-}

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

newtype G s a = G (s -> (s, a))
runSte' :: Mon (Ste s) a -> G s a
runSte' = run (\x -> G (\s -> (s, x))) _alg

steAlg :: Ste s a -> (a -> G s b) -> G s b
steAlg Get     k = k _get
steAlg (Set x) k = _set
