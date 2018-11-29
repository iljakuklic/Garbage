
{-# LANGUAGE RankNTypes, GADTs, NoMonomorphismRestriction, TupleSections #-}

import Control.Applicative

type Algebra f a = f a -> a

data Fix f = Fix { unFix :: f (Fix f) }

data BTreeF a r = Nil | Node a r r deriving (Show)

type BTree a = Fix (BTreeF a)

instance Functor (BTreeF a) where
  fmap f Nil = Nil
  fmap f (Node x l r) = Node x (f l) (f r)


data Cata f a = forall b . Cata (f b -> b) (b -> a)

nil = Fix Nil
node x a b = Fix (Node x a b)

instance Functor (Cata f) where
  fmap f (Cata alg out) = Cata alg (f . out)


instance Functor f => Applicative (Cata f) where
  pure x = Cata (const ()) (\() -> x)
  Cata alga outa <*> Cata algb outb = Cata algc outc
    where
      algc x = (alga $ fmap fst x, algb $ fmap snd x)
      outc (f, x) = outa f (outb x)


cata' :: Functor f => (f b -> b) -> Fix f -> b
cata' alg = alg . fmap (cata' alg) . unFix

cata :: Functor f => Cata f a -> Fix f -> a
cata (Cata alg out) = out . cata' alg

btAlg fNil fNode = Cata alg id
  where
    alg Nil = fNil
    alg (Node x l r) = fNode x l r

add3 a b c = a + b + c
sumNodes = btAlg add3 0
nodeCount = btAlg (\_ -> add3 1) 0
nodeAvg = (/) <$> nodeCount <*> sumNodes

ex1 = node 5.3 (node 7 nil (node 1 nil nil)) (node 2 nil nil)


_1 :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
_1 f (a, b) = (,b) <$> f a
_2 :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
_2 f (a, b) = (a,) <$> f b

_1' :: (forall a b . (a -> b) -> f a -> f b) -> (a -> f b) -> (a, c) -> f (b, c)
_1' fm f (a, b) = (,b) `fm` f a

both f (a, b) = (,) <$> f a <*> f b

dup x = (x, x)
bimap f g (a, b) = (f a, g b)

every f [] = pure []
every f (x:xs) = (:) <$> f x <*> every f xs
askVal f x = fmap (f x) $ putStr ("WTF " ++ show x ++ "? ") >> readLn
