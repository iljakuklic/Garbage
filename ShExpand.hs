{-# LANGUAGE FlexibleInstances, GADTs #-}

import Control.Applicative
import Control.Monad
import Data.Traversable

class ShExpand a where
    shExpand :: a -> IO [String]
    shExpand = shExpandList . pure
    shExpandList :: [a] -> IO [String]
    shExpandList = fmap concat . traverse shExpand



instance ShExpand a => ShExpand [a] where
    shExpandList = fmap concat . traverse shExpandList
    adas

instance (ShExpand a, ShExpand b) => ShExpand (a, b) where
    shExpand (a, b) = shExpand a <> shExpand b

instance (ShExpand a, ShExpand b, ShExpand c) => ShExpand (a, b, c) where
    shExpand (a, b, c) = shExpand a <> shExpand b <> shExpand c

instance ShExpand a => ShExpand (IO a) where
    shExpand cmd = cmd >>= shExpand

instance ShExpand Integer where
    shExpand = pure . pure . show

instance ShExpand Char where
    shExpandList = pure . pure

(><) :: IO [String] -> IO [String] -> IO [String]
(><) = (liftA2 . liftA2) (<>)
