-- Calculator supporting expression over user-defined Clifford algebras.

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified Data.Array.IArray as A
import qualified Data.Array.Unboxed as U
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Char (isDigit)
import Data.List (tails)

data Squaring = SqZero | SqOne | SqNeg | SqSquare deriving (Eq, Show)
newtype Sym = Sym Int deriving (Eq, Ord, Show, U.Ix)

data ClifDesc = ClifDesc {
  cdSymbols :: [String],
  cdSquaring :: [Squaring],
  cdAnticommutes :: [[Bool]]
}

-- Basis tracker with multiplicity at each position
type Basis = [Int]
-- Multivector
data MV a = MV (M.Map Basis a) deriving (Eq, Show, Functor)
unMV (MV x) = x
-- Clif monad: reader providin asscess to a ClifDesc
type Clif = Reader ClifDesc

makeMV :: (Eq a, Num a) => M.Map Basis a -> MV a
makeMV = MV . M.filter (/= 0)
normMV :: (Eq a, Num a) => MV a -> MV a
normMV = makeMV . unMV

-- Multiply two bases, return a new base and its coefficient multiplier.
mulBasis :: Num a => Basis -> Basis -> Clif (Basis, a)
mulBasis bx by = do
    aca <- asks cdAnticommutes
    sqr <- asks cdSquaring
    -- calculate parities
    let (bxp, byp) = (fmap odd bx, fmap odd by)
    let doFlip = foldr (/=) False $ do
        (ac, tbxp) <- zip aca (drop 1 (tails bxp))
        zipWith3 (\a x y -> a && (x /= y)) ac tbxp byp
    let items = zipWith3 (\s x y -> normSym s (x + y)) sqr bx by
    let coef = product (map fst items)
    return (fmap snd items, if doFlip then -coef else coef)
  where
    normSym SqZero x = (0, 0)
    normSym SqOne x = (1, if odd x then 1 else 0)
    normSym SqNeg x = case rem x 4 of
        0 -> (1, 0)
        1 -> (1, 1)
        2 -> (-1, 0)
        3 -> (-1, 1)
    normSym SqSquare x = (1, x)

mulBlades :: Num a => (Basis, a) -> (Basis, a) -> Clif (Basis, a)
mulBlades (xb, x) (yb, y) = fmap (\(zb, m) -> (zb, m * x * y)) (mulBasis xb yb)

-- Basic operations

unit :: Num a => String -> Clif (MV a)
unit s = fmap (\syms -> MV $ M.singleton [if s == s' then 1 else 0 | s' <- syms] 1) (asks cdSymbols)

scalar :: (Eq a, Num a) => a -> Clif (MV a)
scalar x = fmap (\s -> makeMV $ M.singleton [0 | _ <- s] x) (asks cdSquaring)

add :: (Num a, Eq a) => MV a -> MV a -> MV a
add (MV x) (MV y) = makeMV (M.unionWith (+) x y)

neg :: (Num a, Eq a) => MV a -> MV a
neg = fmap negate

mul :: (Num a, Eq a) => MV a -> MV a -> Clif (MV a)
mul (MV xm) (MV ym) = fmap (makeMV . M.fromListWith (+)) $
    sequence [ (mulBlades xb yb) | xb <- M.assocs xm, yb <- M.assocs ym]

-- Experiments

imagDesc = ClifDesc ["i"] [SqNeg] [[]]

i = unit "i"
j = unit "j"
k = unit "k"

-- Printing
shMultivec :: (Eq a, Ord a, Num a, Show a) => MV a -> Clif String
shMultivec (MV mv) | M.null mv = pure "+ 0"
shMultivec (MV mv) = asks cdSymbols >>= \syms -> pure (drop 1 $ M.foldMapWithKey (shBlade syms) mv)
  where
    shSign x | x < 0 = " - "
    shSign _ = " + "
    shBlade syms b x = shSign x ++ show (abs x) ++ concat (zipWith shBasis syms b)
    shBasis _ 0 = ""
    shBasis s 1 = ' ' : s
    shBasis s n = ' ' : (s ++ ('^' : show n))

runClif :: ClifDesc -> Clif a -> a
runClif = flip runReader

interp :: (Num a, Eq a, Read a) => String -> Clif [MV a]
interp s = foldl (\ i c -> i >>= interpCmd c) (pure []) (words s)

--interpCmdS c xs = asks cdSymbols >>= \syms -> _
interpCmd :: (Read a, Eq a, Num a) => String -> [MV a] -> Clif [MV a]
interpCmd c xs | all isDigit c = fmap (:xs) (scalar (read c))
interpCmd "+" (y:x:zs) = pure (add x y : zs)
interpCmd "-" (y:x:zs) = pure (add x (neg y) : zs)
interpCmd "*" (y:x:zs) = fmap (:zs) (mul x y)
interpCmd "~" (y:x:zs) = pure (x:y:zs)
interpCmd "!" (x:zs) = pure zs
interpCmd "$" (x:zs) = pure (x:x:zs)
interpCmd _ _ = error "Wrong command or input"
