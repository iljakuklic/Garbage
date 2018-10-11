{-# LANGUAGE GADTs #-}

import Prelude hiding ((.), id)
import Control.Category as Cat
import Control.Arrow

data FreeCat f a b where
  Id :: FreeCat f a a
  Comp :: f b c -> (FreeCat f a b) -> FreeCat f a c

instance Category (FreeCat f) where
  id = Id
  Id . cat = cat
  Comp a p . q = Comp a (p . q)

runFreeHask :: FreeCat (->) a b -> a -> b
runFreeHask Id = id
runFreeHask (Comp a p) = a . runFreeHask p
