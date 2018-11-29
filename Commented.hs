{-# LANGUAGE NoMonomorphismRestriction, LambdaCase #-}

import Data.Monoid
import Data.Maybe

-- corresponds to: cRemoved (*/cPreserved)? (/*)?
data Commented m = Commented
    { cRemoved :: m
    , cPreserved :: Maybe m
    , cOpen :: Bool
    } deriving (Eq, Show)

unMaybe :: Monoid m => Maybe m -> m
unMaybe = fromMaybe mempty

instance Monoid m => Monoid (Commented m) where
    mempty = Commented mempty Nothing False
    --          a (*/b)? /*        c  */d   (/*)?
    mappend (Commented a b True) (Commented _ (Just d) o)
        = Commented a (b <> Just d) o
    --          a (*/b)? /*        c        (/*)?
    mappend (Commented a b True) (Commented _ Nothing _)
        = Commented a b True
    --          a  */b             c (*/d)? (/*)?
    mappend (Commented a (Just b) False) (Commented c d o)
        = Commented a (Just (b <> c <> unMaybe d)) o
    --          a                  c (*/d)? (/*)?
    mappend (Commented a Nothing False) (Commented c d o)
        = Commented (a <> c) d o


openComment = Commented mempty Nothing True
closeComment = Commented mempty (Just mempty) False
internal x = Commented x Nothing False
deComment (Commented a b _) = a <> unMaybe b

foo True = 5

-- TESTS
charP '[' = openComment
charP ']' = closeComment
charP n = internal [n]

testC :: String -> Commented String
testC = foldMap charP
testS = deComment . testC
allSplits str = fmap (flip splitAt str) [0..length str]
check x = and [ testC a <> testC b == testC x | (a, b) <- allSplits x ]

test1 = foldMap $ \case
    '[' -> openComment
    ']' -> closeComment
    '+' -> internal (Sum 1)
    '-' -> internal (Sum (negate 1))
    _   -> mempty
