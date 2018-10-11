{-# LANGUAGE
      RankNTypes, TypeFamilies, MultiParamTypeClasses,
      FunctionalDependencies, FlexibleInstances, FlexibleContexts,
      NoMonomorphismRestriction #-}

data Expr var
  = Const Integer
  | Var var
  | Add (Expr var) (Expr var)
  | Neg (Expr var)
  | Mul (Expr var) (Expr var)
  deriving (Show)

instance Num (Expr var) where
  (+) = Add
  (*) = Mul
  negate = Neg
  fromInteger = Const
  abs = error "'abs' not supported"
  signum = error "'signum' not supported"

data Closure var
  = Arg (var -> Closure var)
  | Body (Expr var)

arg :: (Expr var -> Closure var) -> Closure var
arg f = Arg (f . Var)

newtype Func = Func { getFunc :: forall var . Closure var }

-- Printing
shExpr (Const n) = show n
shExpr (Var n)   = [n]
shExpr (Add x y) = "(" ++ shExpr x ++ " + " ++ shExpr y ++ ")"
shExpr (Neg x)   = "(-" ++ shExpr x ++ ")"
shExpr (Mul x y) = shExpr x ++ " * " ++ shExpr y

shClosureN n (Body expr) = " ) -> " ++ shExpr expr
shClosureN n (Arg f) = " " ++ [n] ++ shClosureN (succ n) (f n)

instance (v ~ Char) => Show (Closure v) where
  show = (" (" ++) . shClosureN 'a'
instance Show Func where
  show = show . getFunc

-- Convert functons to Func

class ToClosure a v | a -> v where
  toClosure :: a -> Closure v

instance ToClosure (Expr v) v where
  toClosure = Body

instance ToClosure b v => ToClosure (Expr v -> b) v where
  toClosure f = Arg (toClosure . f . Var)

--toFunc :: (forall v . ToClosure a v => a) -> Func
--toFunc x = Func (toClosure x)

{-
class ToClosure a where
  type VarTy a :: *
  toClosure :: a -> Closure (VarTy a)

instance ToClosure (Expr var) where
  type VarTy (Expr var) = var
  toClosure = Body

instance (a ~ Expr (VarTy b), ToClosure b) => ToClosure (a -> b) where
  type VarTy (a -> b) = VarTy b
  toClosure f = Arg (toClosure . f . Var)
-}

-- TODO how to type this
--toFunc :: (ToClosure a, forall var . var ~ VarTy a) => a -> Func
--toFunc = Func . toClosure

-- Examples

ex1 :: Num a => a -> a -> a
ex1 x y = x * x + y * y

ex2 = arg $ \x -> arg $ \y -> Body $ x * x + y * y
