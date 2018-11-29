{-# LANGUAGE NoMonomorphismRestriction, RankNTypes, GADTs, PartialTypeSignatures #-}

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Data.Void


data TermDB v
    = VarDB v
    | AppDB (TermDB v) (TermDB v)
    | LamDB (TermDB (Maybe v))
    deriving (Eq, Show)

instance Functor TermDB where
    fmap f (VarDB x) = VarDB (f x)
    fmap f (AppDB a b) = AppDB (fmap f a) (fmap f b)
    fmap f (LamDB e) = LamDB (fmap (fmap f) e)

instance Foldable TermDB where
    foldMap f (VarDB v) = f v
    foldMap f (AppDB a b) = foldMap f a <> foldMap f b
    foldMap f (LamDB t) = foldMap (maybe mempty f) t

instance Traversable TermDB where
    sequenceA (VarDB v) = fmap VarDB v
    sequenceA (AppDB a b) = liftA2 AppDB (sequenceA a) (sequenceA b)
    sequenceA (LamDB ft) = fmap LamDB (sequenceA (fmap sequenceA ft))

closed :: Traversable f => f a -> Maybe (f b)
closed = traverse (const Nothing)

-- Abstract: replace free var B with a bound one.
bindDB :: (Eq v) => v -> (TermDB v) -> TermDB v
bindDB v = LamDB . fmap (\v' -> if v' == v then Nothing else Just v')

testd1, testd2 :: TermDB Char
testd1 = LamDB (AppDB (VarDB Nothing) (VarDB (Just 'x')))
testd2 = bindDB 'x' testd1
testd3 = bindDB 'x' (bindDB 'f' (AppDB (VarDB 'f') (VarDB 'x')))
testd4 = LamDB (LamDB (AppDB (VarDB Nothing) (VarDB (Just Nothing))))

data TermPH b v
    = VarPH v
    | AppPH (TermPH b v) (TermPH b v)
    | LamPH (b -> TermPH b v)

instance Functor (TermPH bn) where
    fmap f (VarPH v) = VarPH (f v)
    fmap f (AppPH a b) = AppPH (fmap f a) (fmap f b)
    fmap f (LamPH tf) = LamPH (fmap f . tf)

{-
foldPH :: Monoid m => (a -> m) -> TermPH a' a -> m
foldPH f (VarPH v) = f v
foldPH f (AppPH a b) = foldPH f a <> foldPH f b
foldPH f (LamPH tf) = foldPH f (tf _)
-}

db2ph :: TermDB v -> TermPH v v
db2ph (VarDB v) = VarPH v
db2ph (AppDB a b) = AppPH (db2ph a) (db2ph b)
db2ph (LamDB tt) = LamPH (\v -> db2ph (fmap (fromMaybe v) tt))

ph2db :: Eq v => [v] -> TermPH v v -> TermDB v
-- TODO BUG filter out used names before running this
ph2db _ (VarPH v) = VarDB v
ph2db ns (AppPH a b) = AppDB (ph2db ns a) (ph2db ns b)
ph2db (n:ns) (LamPH tt) = bindDB n (ph2db ns (tt n))

data TermBF bv fv
    = VarB bv
    | VarF fv
    | AppBF (TermBF bv fv) (TermBF bv fv)
    | LamBF (bv -> TermBF bv fv)

instance Functor (TermBF bv) where
    fmap f (VarB x) = VarB x
    fmap f (VarF x) = VarF (f x)
    fmap f (AppBF a b) = AppBF (fmap f a) (fmap f b)
    fmap f (LamBF tf) = LamBF (fmap f . tf)

instance Applicative (TermBF bv) where
    pure = VarF
    (<*>) = ap

instance Monad (TermBF bv) where
    VarB x >>= f = VarB x
    VarF x >>= f = f x
    AppBF a b >>= f = AppBF (a >>= f) (b >>= f)
    LamBF tf >>= f = LamBF (\v -> tf v >>= f)

instance Foldable (TermBF bv) where
    foldMap f (VarB _) = mempty
    foldMap f (VarF x) = f x
    foldMap f (AppBF a b) = foldMap f a <> foldMap f b
    foldMap f (LamBF tf) = foldMap f (tf (error "Should not happen")) -- HACK

instance Traversable (TermBF bv) where
    sequenceA (VarF x) = fmap VarF x
    sequenceA (VarB b) = pure (VarB b)
    sequenceA (AppBF a b) = liftA2 AppBF (sequenceA a) (sequenceA b)
    sequenceA (LamBF tt) = sequenceA (tt (error "No idea how to do this"))

showBF :: Show fv => Char -> (TermBF Char fv) -> String
showBF _ (VarB x) = "(VarB v" ++ (x : ")")
showBF _ (VarF x) = "(VarF (" ++ show x ++ "))"
showBF n (AppBF a b) = "(AppBF " ++ showBF n a ++ " " ++ showBF n b ++ ")"
showBF n (LamBF tf) = "(LamBF $ \\v" ++ n : " -> " ++ showBF (succ n) (tf n) ++ ")"

instance (Show fv, bv ~ Char) => Show (TermBF bv fv) where
    show = showBF 'a'

seqA :: Applicative f => TermBF (TermBF bv fv) (f fv) -> f (TermBF bv fv)
seqA (VarF x) = fmap VarF x
seqA (VarB x) = pure x
seqA (AppBF a b) = liftA2 AppBF (seqA a) (seqA b)
seqA (LamBF tt) = fmap LamBF (fmap const (seqA (tt (error "No idea what I am doing"))))

trav f x = seqA (fmap f x)
cls = trav (const Nothing)

unbound :: TermBF () fv -> Maybe (TermBF bv fv)
unbound (VarF x) = Just (VarF x)
unbound (VarB ()) = Nothing
unbound (AppBF a b) = liftA2 AppBF (unbound a) (unbound b)
unbound (LamBF tf) = fmap (LamBF . const) (unbound (tf ()))

order1 :: Int -> TermBF Int fv -> (TermBF bv (Either Int fv))
order1 n (VarF x) = VarF (Right x)
order1 n (VarB x) = VarF (Left x)
order1 n (AppBF a b) = AppBF (order1 n a) (order1 n b)
order1 n (LamBF tf) = LamBF (const $ order1 (succ n) (tf n))

orderN :: Int -> (TermBF bv (Either Int fv)) -> TermBF bv fv
orderN n (VarF (Right x)) = (VarF x)
orderN n (VarF (Left x)) = error "This is not Right"
orderN n (VarB v) = (VarB v)
orderN n (AppBF a b) = AppBF (orderN n a) (orderN n b)
orderN n (LamBF tf) = LamBF (\v -> orderN (succ n) (tf v >>= bindBFL n v))

bindBF :: Eq fv => fv -> TermBF bv fv -> TermBF bv fv
bindBF n tt = LamBF (\b -> tt >>= \v -> if v == n then VarB b else VarF v)
bindBFL :: Eq a => a -> bv -> Either a b -> TermBF bv (Either a b)
bindBFL n v = either (\n' -> if n' == n then VarB v else VarF (Left n')) (VarF . Right)
--bindIx n = _

substNothing :: TermBF bv fv -> TermBF (Maybe bv) fv -> TermBF bv fv
substNothing v (VarF x) = VarF x
substNothing v (VarB (Just x)) = VarB x
substNothing v (VarB Nothing) = v
substNothing v (AppBF a b) = AppBF (substNothing v a) (substNothing v b)
substNothing v (LamBF tf) = LamBF (substNothing v . tf . Just)

seq1 :: Applicative f => TermBF () (Either Int (f fv)) -> f (TermBF bv (Either Int fv))
seq1 (VarF (Left n)) = pure (VarF (Left n))
seq1 (VarF (Right x)) = fmap (VarF . Right) x
seq1 (VarB _) = error "Nothing should be bound here"
seq1 (AppBF a b) = liftA2 AppBF (seq1 a) (seq1 b)
seq1 (LamBF tf) = fmap (LamBF . const) (seq1 (tf ()))

seqBF = fmap (orderN 0) . seq1 . order1 0

{-
seqN :: Applicative f => Int -> TermBF Int (f fv) -> f (TermBF bv fv)
-- seqN n x = fmap (orderN n) (seq1 (order1 n x))
seqN n (VarF x) = fmap VarF x
seqN n (VarB x) = pure (error "There should not be bound vars at this point")
seqN n (AppBF a b) = liftA2 AppBF (seqN n a) (seqN n b)
--seqN n (LamBF tf) = fmap (orderN n) (seq1 (order1 n (LamBF tf)))
--seqN n (LamBF tf) = LamBF (const $ _ (succ n) (tf n))
seqN n (LamBF tf) = let t = tf n in seqN (succ n) (LamBF $ \v -> _)
  -- fmap fg (seq1 (order1 (succ n) (tf n)))
  where
      fg x = LamBF (\v -> orderN (succ n) (x >>= bindBFL n v))
      bindBFL n v = either (\n' -> if n' == n then VarB v else VarF (Left n')) (VarF . Right)
-}


inOrder1 f = orderN 0 . f . order1 0


testb1 = (LamBF $ \x -> VarB x)
testb2 = (LamBF $ \x -> LamBF $ \f -> AppBF (VarB f) (VarB x))
testb3 = AppBF (VarF 'f') (VarF 'x')
testb4 = bindBF 'x' $ bindBF 'f' $ testb3
testb5 = order1 0 testb4
testb6 = orderN 0 testb5
