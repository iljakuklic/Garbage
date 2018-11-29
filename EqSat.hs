{-# LANGUAGE NoMonomorphismRestriction, LambdaCase,
             DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving,
			 RankNTypes, GADTs, FlexibleContexts #-}

import qualified Data.IntMap.Strict    as M
import qualified Data.Set              as S
import qualified Data.Functor.Identity as F
import qualified Data.Functor.Constant as F
import           Data.Foldable
import           Data.Traversable
import           Control.Applicative
import           GHC.Show
import           Data.String

data Prim
    = Call String  -- function call
    -- combinatory logic
	| I  -- id    = \     v -> v
    | K  -- const = \ e   v -> e
    | S  -- ap    = \ f x v -> (f v) (x v)
    | B  -- (.)   = \ f x v -> (f  ) (x v)
    | C  -- flip  = \ f x v -> (f v) (x  )
	-- arithmetic
    | Add  -- (+)
    | Mul  -- (*)
    | Neg  -- negate
	-- control flow
	| Ret     -- lift pure value: return :: a -> IO a
    | Exit    -- exit :: Int -> IO a
	-- mutable references
    | Alloca  -- alloca :: (MutVar -> IO a) -> (a -> IO b) -> IO b
    | Read    -- read :: MutVar -> (Int -> IO a) -> IO a
    | Write   -- write :: MutVar -> Int -> IO a -> IO a
    deriving (Eq, Show)

data TermF p r = Prim p | App r r
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Graph f = Graph { unGraph :: M.IntMap (f Int) }

-- graph operations
showGraph = unlines . map (\(k, v) -> show k ++ " -> " ++ show v) . M.assocs . get graphRep
printGraph = putStr . showGraph
nextIdx = maybe 0 (succ . fst . fst) . M.maxViewWithKey . unGraph
emptyGraph = Graph M.empty
fromList = Graph . M.fromList
insert val g@(Graph rep) = let idx = nextIdx g in (idx, Graph (M.insert idx val rep))
indexOf val (Graph rep) = fmap fst $ find (\(k,v) -> v == val) (M.assocs rep)
insertUniq val g = maybe (insert val g) (\k -> (k, g)) (indexOf val g)

-- some lens combinators
type Lens f s t a b = (a -> f b) -> (s -> f t)
over :: Lens F.Identity s t a b -> (a -> b) -> (s -> t)
over l f s = F.runIdentity $ l (F.Identity . f) s
set :: Lens F.Identity s t a b -> b -> (s -> t)
set l x = over l (const x)
get :: Lens (F.Constant a) s t a b -> s -> a
get l = F.getConstant . l F.Constant

-- graph lenses
iso from to l x = from <$> l (to x)
beside l1 l2 l (x, y) = (,) <$> l1 l x <*> l2 l y
graphRep = iso Graph unGraph
polySetElems = iso S.fromList S.toList . traverse
intMapElems f = iso (M.fromListWith f) M.toList . traverse
graphIdxs merge gl = graphRep . intMapElems merge . beside id gl

-- access terms
subterms l (Prim s) = pure (Prim s)
subterms l (App a b) = App <$> l a <*> l b

type TermGraph p = Graph (TermF p)

newtype TermEqF p a = TermEqF { unTermSetF :: S.Set (TermF p a) }
    deriving (Eq, Ord)

instance (Show p, Show a) => Show (TermEqF p a) where
	show = show . S.toList . unTermSetF

type TermEqGraph p = Graph (TermEqF p)

fromTermGraph = over (graphRep . traverse) (TermEqF . S.singleton)
termSetRep = iso TermEqF unTermSetF
unionTermSets (TermEqF s1) (TermEqF s2) = TermEqF (S.union s1 s2)
termEqGraphIdxs = graphIdxs unionTermSets (termSetRep . polySetElems . traverse)
equateSets ix1 ix2 = over termEqGraphIdxs (\i -> if i == ix2 then ix1 else i)

-- explicit term tree representation
newtype TermTree p = TT (TermF p (TermTree p)) deriving (Eq, Ord)

p = TT . Prim
infixl 1 @@
t @@ u = TT (App t u)
op1 nme x     = p nme @@ x
op2 nme x y   = p nme @@ x @@ y
op3 nme x y z = p nme @@ x @@ y @@ z

insertTT (TT (Prim x)) g = insertUniq (Prim x) g
insertTT (TT (App t u)) g0 = insertUniq (App tIdx uIdx) g2
  where (tIdx, g1) = insertTT t g0
        (uIdx, g2) = insertTT u g1
fromTermTree = flip insertTT emptyGraph

instance Show p => Show (TermTree p) where
    showsPrec _ (TT (Prim p)) = shows p
    showsPrec p (TT (App t u)) = showParen (p > 0) $ showsPrec 0 t . showLitChar ' ' . showsPrec 1 u
instance IsString p => IsString (TermTree p) where
    fromString = TT . Prim . fromString
instance (p ~ String) => Num (TermTree p) where
    (+) = op2 "add"
    (*) = op2 "mul"
    abs = op1 "abs"
    signum = op1 "signum"
    fromInteger = p . show

