{-# LANGUAGE NoMonomorphismRestriction, RankNTypes #-}

import Data.Functor
import Data.Functor.Identity
import Data.Functor.Constant
import Control.Applicative

-- LENS: (b -> f b') -> (a -> f a')

lens :: Functor f => (a -> b) -> (b' -> a') -> (b -> f b') -> (a -> f a')
lens ab ba l = fmap ba . l . ab

-- ------------------------------------------------------------------

fst_ :: Functor f => (a -> f a') -> (a, b) -> f (a', b)
fst_ l (x, y) = (\x' -> (x', y)) <$> l x

-- fst_' l p@(x, y) = lens (const x) (\x' -> (x', y)) l p

swapped :: Functor f => ((b, a) -> f (b', a')) -> (a, b) -> f (a', b')
swapped = let f (x, y) = (y, x) in lens f f

snd_ :: Functor f => (b -> f b') -> (a, b) -> f (a, b')
snd_ = swapped . fst_

both :: Applicative f => (a -> f a') -> (a, a) -> f (a', a')
both l (x, y) = (,) <$> l x <*> l y
also l r (x, y) = (,) <$> l x <*> r y


left_ :: Applicative f => (l -> f l') -> Either l r -> f (Either l' r)
left_ l = either (fmap Left . l) (pure . Right)

flipped :: Functor f => (Either b a -> f (Either b' a')) -> Either a b -> f (Either a' b')
flipped = let f = either Right Left in lens f f

right_ :: Applicative f => (r -> f r') -> Either l r -> f (Either l r')
right_ = flipped . left_


when :: Applicative f => (a -> Bool) -> (a -> f a) -> (a -> f a)
when p l x = if p x then l x else pure x


scaled n = lens (*n) (/n)
negated = lens negate negate
polar = lens (\(x, y) -> (sqrt (x^2 + y^2), atan2 y x)) (\(l, a) -> (l * cos a, l * sin a))

-- ------------------------------------------------------------------

type LensP f s s' a a' = (a -> f a') -> (s -> f s')
type Lens  f s a = LensP f s s a a

($*$) :: Applicative f => Lens f s a -> Lens f r a -> Lens f (s, r) a
(as $*$ ar) afa (s, r) = (,) <$> as afa s <*> ar afa r

keep :: Applicative f => Lens f a x
keep = const pure

the :: Functor f => a -> Lens f s a
the a l s = s <$ l a -- s needed here??
the1 :: Functor f => a -> LensP f b c a c
the1 a l s = l a -- NOPE              ^ wrong position

(>*<) :: Applicative f => LensP f s s (a -> b) a' -> LensP f s s a a' -> LensP f s s b a'
(af >*< ax) afa x = undefined

-- ------------------------------------------------------------------

newtype Getter a b = Getter { runGetter :: a }
instance Functor (Getter a) where fmap _f = Getter . runGetter

get :: ((b -> Getter b x) -> a -> Getter b' y) -> a -> b'
get l s = runGetter $ l Getter s

--app :: (b -> b') -> ((b -> Identity b') -> a -> Identity a') -> a -> a'
app l s f = runIdentity $ l (Identity . f) s

set l s x = app l s (const x)
infix 2 <~
infixl 1 #
l <~ x = \s -> set l s x
x # f = f x
