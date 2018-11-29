
{-# LANGUAGE RankNTypes #-}

class Profunctor p where
  pmap :: (a -> b) -> (d -> c) -> p c a -> p d b

data ExpF var exp = exp :$ exp | Lam (var -> exp)

instance Profunctor ExpF where
  pmap f g (x :$ y) = f x :$ f y
  pmap f g (Lam ef) = Lam (f . ef . g)

pfmap f = pmap f id
pcmap f = pmap id f

data WHAT = WHAT

data Rec p a b = Var b | Rec (p a (Rec p a b))

instance Profunctor p => Profunctor (Rec p) where
  pmap ab dc (Var x) = Var (ab x)
  pmap ab dc (Rec x) = Rec (pmap (pmap ab dc) dc x)

instance Profunctor p => Monad (Rec p a) where
  return = Var
  --x >>= f = cata Rec $ pfmap f x
  Var x >>= f = f x
  Rec x >>= f = Rec $ pfmap (>>= f) x

cata :: Profunctor p => (p a b -> b) -> Rec p a b -> b
cata alg (Var x) = x
cata alg (Rec x) = alg $ pfmap (cata alg) x


type Exp b = Rec ExpF b b

cataE :: (ExpF a b -> b) -> Rec ExpF a b -> b
cataE = cata

var = Var
lam v = Rec (Lam v)
x $$ y = Rec (x :$ y)

idE = lam $ \x -> var x
koE = lam $ \x -> lam $ \_ -> var x
tE  = lam $ \t -> lam $ \f -> var t
fE  = lam $ \t -> lam $ \f -> var f
koidE = lam $ \b -> lam $ \a -> var a

koC = \x -> lam $ \_ -> var x

eval :: (forall b . Exp b) -> Exp a
eval (Rec (Rec (Lam f) :$ x)) = cata Rec (f x)
eval x = x

nVars = cata f where
  f (x :$ y) = x + y
  f (Lam ef) = ef 1

nAbs = cata f where
  f (x :$ y) = x + y
  f (Lam ef) = ef 0 + 1

prExp = putStrLn . shExp
shExp e = cata f e names where
  f :: ExpF ([String] -> String) ([String] -> String) -> [String] -> String
  f (x :$ y) n = "(" ++ x n ++ " " ++ y n ++ ")"
  f (Lam ef) (v:vs) = "(\\" ++ v ++ " -> " ++ ef (const v) vs ++ ")"
  names = map return ['a'..'z'] ++ ['x': show n | n <- [1..]]

