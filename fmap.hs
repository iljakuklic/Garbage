{-# LANGUAGE NoMonomorphismRestriction, RankNTypes #-}


type FMap f = forall a b . (a -> b) -> (f a -> f b)

fmapList :: FMap []
fmapList = fmap

fmapMaybe :: FMap Maybe
fmapMaybe = fmap

type C f g x = f (g x)
type K a x = a

fmapComp :: FMap f -> FMap g -> FMap (C f g)
fmapComp f g = f . g

fmapConst :: FMap (K c)
fmapConst _ x = x
