
{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction #-}

import Prelude hiding ((>>=), return, fmap)
import Data.Monoid

class IxMonad m where
  return :: a -> m i i a
  (>>=) :: m i j a -> (a -> m j k b) -> m i k b

infixl 1 >>=
infixl 4 <$>
infixl 4 <*>

fmap f = (>>= return . f)
pure = return
(<$>) = fmap
ff <*> xx = do f <- ff; x <- xx; return (f x)

newtype Cont i o a = Cont { runCont :: ((a -> o) -> i) }

instance IxMonad Cont where
  return x = Cont ($ x)
  Cont c >>= f = Cont $ \k -> c (flip runCont k . f)

now :: a -> Cont i i a
now = return
hole :: (a -> b) -> Cont (a -> o) o b
hole f = Cont (. f)
fill :: Cont a r r -> a
fill = flip runCont id

a <<>> b = (<>) <$> a <*> b
a .<>> b = now a <<>> b
a ^<>> b = hole a <<>> b

{-
a <% b = a <<>> hole b
a %> b = a <<>> now b
a <%%> b = a <% id %> b
a <%$%> b = a <% show %> b
fmt = now mempty
-}

a @$ b = a <$> hole b
a @? b = a <*> hole b
a @= b = a <*> now b

(%) = (@?)
(#) = (@=)
(<%) = (@$)
(<@?) = (@$)

data WHAT = WHAT
