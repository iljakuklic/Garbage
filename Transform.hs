{-# LANGUAGE NoMonomorphismRestriction, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Bound
import Control.Applicative
import Prelude.Extras
import Data.Foldable
import Data.Traversable

-- primitive operation
data PrimOpZ = Add | Sub | Neg | Mul | Inv | Div | Abs  -- arithmetic
             | Shl | Shr | And | Or  | Xor              -- bit-level
			 | Ifn | Ifp | Ifz                          -- branching
	deriving (Eq, Show)

-- type
data TypeZ = TInt | TypeZ :-> TypeZ
	deriving (Eq, Show)

-- expression
data ExpZ a = Var a
            | Lit Integer
            | Let TypeZ (ExpZ a) (Scope () ExpZ a)
			| Prim PrimOpZ
			| ExpZ a :@ ExpZ a
	deriving (Eq, Show, Functor, Foldable, Traversable)

instance Eq1 ExpZ
instance Show1 ExpZ

mkPrim1 p x = Prim p :@ x
mkPrim2 p x y = Prim p :@ x :@ y
mkPrim3 p x y z = Prim p :@ x :@ y :@ z
ifn = mkPrim3 Ifn
c -? (t, e) = ifn c t e
x = Var "x"
y = Var "y"
z = Var "z"

instance Num (ExpZ a) where
	fromInteger = Lit
	(+) = mkPrim2 Add
	(-) = mkPrim2 Sub
	(*) = mkPrim2 Mul
	negate = mkPrim1 Neg
	abs = mkPrim1 Abs
	signum = undefined

instance Applicative ExpZ where
	pure = Var
	Var f <*> e = fmap f e
	Lit x <*> _ = Lit x
	Prim p <*> _ = Prim p
	(ef :@ ex) <*> e = (ef <*> e) :@ (ex <*> e)

instance Monad ExpZ where
	return = Var
	Var x >>= f = f x
	Lit n >>= f = Lit n
	(ef :@ ex) >>= f = (ef >>= f) :@ (ex >>= f)
	Prim p >>= f = Prim p


e1 = Prim Add :@ Var 'x' :@ Var 'y'

