{-# LANGUAGE NoMonomorphismRestriction, RankNTypes #-}

-- Entity-component system map experiment

import qualified Data.IntMap as M
import Control.Applicative
import Data.Foldable
import Data.Traversable

type Entity = M.Key
data Component a = Component (Maybe a) (M.IntMap a) deriving Show

get :: Entity -> Component a -> Maybe a
get e (Component def comp) = M.lookup e comp <|> def

instance Functor Component where
  fmap f (Component d m) = Component (fmap f d) (fmap f m)

instance Applicative Component where
  pure x = Component (pure x) M.empty
  Component fd fc <*> Component xd xc = Component (fd <*> xd) comp
    where
      comp = M.mergeWithKey (\_ f x -> Just (f x)) fonly xonly fc xc
      fonly = M.mapMaybe (\f -> fmap f xd)
      xonly = M.mapMaybe (\x -> fmap ($x) fd)
 
instance Alternative Component where
  empty = Component Nothing M.empty
  comp@(Component (Just _) _) <|> _ = comp
  Component Nothing xc <|> Component yd yc = Component yd (M.union xc yc)

instance Foldable Component where
  foldMap = foldMapDefault

instance Traversable Component where
  traverse f (Component d c) = Component <$> traverse f d <*> traverse f c
