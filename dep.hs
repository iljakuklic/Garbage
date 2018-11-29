
{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}

import Control.Applicative
import Control.Monad.Trans.RWS
import Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Traversable as T

data Konst = Star | Box
data Binder x = Lam x | Pi x

instance Functor Binder where
  fmap = T.fmapDefault
instance F.Foldable Binder where
  foldMap = T.foldMapDefault
instance T.Traversable Binder where
  traverse f (Lam x) = Lam <$> (f x)
  traverse f (Pi  x) = Pi  <$> (f x)

bindTy  (Lam x) = x
bindTy  (Pi  x) = x
bindSym (Lam _) = "λ"
bindSym (Pi  _) = "Π"
bindArr (Lam _) = "↦"
bindArr (Pi  _) = "→"

instance Show Konst where
  show Star = "∗"
  show Box  = "◻"

data Exp a = KExp Konst
           | BVar a   -- bound variable
           | Binder (Exp a) :. (a -> Exp a)
           | (Exp a) :@ (Exp a)


expJoin :: Exp (Exp a) -> Exp a
expJoin (KExp x) = KExp x
expJoin (BVar x) = x
expJoin (b :. e) = fmap expJoin b :. (expJoin . e . BVar)
expJoin (x :@ y) = expJoin x :@ expJoin y

type CExp = forall b . Exp b
ex :: CExp -> Exp a
ex x = x

subst :: (forall b . b -> Exp b) -> Exp a -> Exp a
subst e x = expJoin (e x)

eta' :: Exp (Maybe a) -> Maybe (Exp a)
eta' (KExp k) = Just $ KExp k
eta' (BVar (Just x)) = Just $ BVar x
eta' (BVar Nothing) = Nothing
eta' (x :@ y) = (:@) <$> eta' x <*> eta' y
eta' (b :. e) = (:.) <$> T.traverse eta' b <*> (_ . e . Just)
--eta'' :: (forall b . b -> Exp (Maybe b)) -> Maybe (a -> Exp a)
--eta'' f x = eta' (f x)

data WHAT = WHAT

fresh pre = ((pre ++) . show) <$> get <* modify succ

parens innerPri action = do
  let addParens x = concat ["(", x, ")"]
  outerPri <- ask
  if innerPri >= outerPri
    then local (const innerPri) action
    else addParens <$> local (const 0) action
assocParensHere = local succ

varNameHint b = case (b, bindTy b) of
  (Lam _, KExp Star) -> "T"
  (Lam _, Pi _ :. _) -> "f"
  _ -> "x"

showExp (KExp k) = pure $ show k
showExp (BVar v) = pure v
showExp (b :. ef) = do
  b' <- showExp (bindTy b)
  x  <- fresh (varNameHint b)
  e' <- showExp (ef x)
  return $ concat [bindSym b, "(", x, " : ", b', ") ", e']
showExp (e1 :@ e2) = parens 1 $ innerSpace <$> showExp e1 <*> (assocParensHere $ showExp e2)
  where innerSpace x y = concat [x, " ", y]
shExp e = let (r, ()) = evalRWS (showExp e) 0 0 in r
prExp = putStrLn . shExp

ty = KExp Star

idT = Pi  ty :. \t -> Pi  (BVar t) :. \x -> BVar t
idE = Lam ty :. \t -> Lam (BVar t) :. \x -> BVar x

a .-> b = Pi a :. \_ -> b

dotE = Lam ty :. \a -> Lam ty :. \b -> Lam ty :. \c ->
       Lam (BVar b .-> BVar c) :. \f -> Lam (BVar a .-> BVar b) :. \g ->
       Lam (BVar a) :. \x -> BVar f :@ (BVar g :@ BVar x)
