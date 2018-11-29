{-# LANGUAGE NoMonomorphismRestriction, NoImplicitPrelude, RebindableSyntax, RankNTypes, KindSignatures, GADTs #-}

import Prelude(($), fromInteger, Show, show, (++), String, succ, unlines, fst, Functor, fmap, (.), Int, Bool(..))

class IFunctor f where
	imap :: (a -> b) -> (f s s a -> f s s b)

class IFunctor f => IApplicative f where
	return :: a -> f s s a
	(<*>) :: f s s' (a -> b) -> f s' s'' a -> f s s'' b

class IApplicative f => IMonad f where
	ijoin :: f s s' (f s' s'' b) -> f s s'' b

m >>= f = ijoin (imap f m)
infixl 1 >>=

newtype State s s' a = State { getState :: s -> (s', a) }

instance IFunctor State where
	imap f (State ste) = State $ \s -> let (s', x) = ste s in (s', f x)

instance IApplicative State where
	return x = State $ \s -> (s, x)
	(State sff) <*> (State sfx) = State $ \s1 -> let (s2, f) = sff s1; (s3, x) = sfx s2 in (s3, f x)

instance IMonad State where
	ijoin (State sf1) = State $ \s1 -> let (s2, State sf2) = sf1 s1; (s3, x) = sf2 s2 in (s3, x)

get = State $ \s -> (s, s)
put x = State $ \s -> (x, ())
xchg x = State $ \s -> (x, s)
modify f = State $ \s -> (f s, ())
modifyx f = State $ \s -> (f s, s)


--class Functor2 f where
	-- map2 :: forall c . (a -> b) -> (f a c -> f b c)


{-
data BinDAGP a b = Nil
                 | Fork a (BinDAGP a b) (BinDAGP a b)
                 | Var b
                 | Let (BinDAGP a b) (b -> BinDAGP a b)

newtype BinDAG a = BinDAG { getBinDAG :: forall b . BinDAGP a b }

leaf x = Fork x Nil Nil
exDAG1 = BinDAG $ Fork 75 (leaf 12) (Let (Fork 5 (leaf 3) (leaf 7)) (\a -> Fork 10 (Let (leaf 3) (\b -> Fork 55 (Var b) (Fork 44 (Var b) (Var a)))) (Var a)))
exDAG2 = BinDAG $ Let (leaf "x") (\x -> Let (leaf "y") (\y -> Fork "t" (Var x) (Var y)))


instance Show a => Show (BinDAG a) where
  show (BinDAG x) = "BinDAG " ++ sh 0 x
    where
      sh i (Fork x Nil Nil) = "(leaf " ++ show x ++ ")"
      sh i Nil = "Nil"
      sh i (Fork x a b) = "(Fork " ++ show x ++ " " ++ sh i a ++ " " ++ sh i b ++ ")"
      sh i (Var x) = 'x' : show x
      sh i (Let x ff) = "(Let " ++ sh i x ++ " (\\x" ++ show i ++ " -> " ++ sh (succ i) (ff i) ++ ")"
-}

data EE

data BD p where
	Nil :: BD p
	Bin :: String -> BD p -> BD p -> BD p
	Var :: x -> BD (x, p)
	Nxt :: BD p -> BD (x, p)
	Let :: BD p -> (forall x . x -> BD (x, p)) -> BD p


