{-# LANGUAGE LambdaCase, ViewPatterns, OverloadedStrings, GADTs #-}

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Foldable
import Data.Traversable
import Data.String
import Data.Monoid hiding (Alt)
import Prelude hiding (seq)

data RE a
  = One a               -- Single char.
  | Seq [RE a]          -- Sequence. Not a singleton, flat
  | Alt (S.Set (RE a))  -- Choice. Not a singleton, flat
  | Star (RE a)         -- Closure. Not trivial (empty or eps)
  deriving (Eq, Ord)

instance Semigroup (RE a) where
  (<>) = mappend

instance Monoid (RE a) where
  mempty = eps
  mappend a b = seq [a, b]

instance (a ~ Char) => IsString (RE a) where
  fromString = str

instance (a ~ Char) => Show (RE a) where
  show = show . showRE

one :: a -> RE a
one = One

none :: RE a
none = Alt S.empty

eps :: RE a
eps = Seq []

oneOf :: Ord a => [a] -> RE a
oneOf = Alt . S.fromList . fmap One

opt :: Ord a => RE a -> RE a
opt x = alt [eps, x]

str :: [a] -> RE a
str = Seq . fmap One

seq :: [RE a] -> RE a
seq res = maybe none (single . mconcat) . flip traverse res $ \case
    Alt res | S.null res -> Nothing
    Seq res -> Just res
    re' -> Just [re']
  where
    single [x] = x
    single xs = Seq xs

alt :: Ord a => [RE a] -> RE a
alt res = single . flip foldMap res $ \case
    Alt res' -> res'
    re -> S.singleton re
  where
    single (S.elems -> [x]) = x
    single xs = Alt xs

alt2 x y = alt [x, y]
seq2 x y = seq [x, y]

star :: RE a -> RE a
star (Alt s) | S.null s = eps
star re@(Seq []) = re
star re@(Star _) = re
star re = Star re

nullable :: RE a -> Bool
nullable (One _) = False
nullable (Seq rs) = all nullable rs
nullable (Alt rs) = any nullable rs
nullable (Star _) = True

nullRE :: RE a -> RE a
nullRE re = if nullable re then eps else none

diffs :: Ord a => RE a -> M.Map a (RE a)
diffs (One x) = M.singleton x eps
diffs (Seq []) = M.empty
diffs (Seq (r:rs)) =
    let sq'  = M.map (\x -> seq (x:rs)) (diffs r)
        rrs' = M.unionWith alt2 sq' (diffs (Seq rs))
    in if nullable r then rrs' else sq'
diffs (Alt as) = M.unionsWith alt2 [ diffs a | a <- S.toList as ]
diffs sre@(Star re) = diffs (seq [re, sre])

digit = oneOf "0123456789"
re055 = alt [ oneOf "01234" <> digit, one '5' <> oneOf "012345" ]
re255 = alt [ oneOf ['1'..'9'] <> opt digit
            , one '1' <> digit <> digit
            , one '2' <> re055 ]
redot255 = re255 <> star (one '.' <> re255)
reip4 = re255 <> mconcat (replicate 3 (one '.' <> re255))
re1 = star (opt (str "ab") <> opt (str "cd") <> str "ef")
bla = star (str "bla")

isOne (One _) = True
isOne _ = False
showRE (One x) = [x]
showRE (Seq res) = concatMap showRE res
showRE (Alt res0) =
  let (ones, res) = S.partition isOne res0
      onestr = "[" ++ [ c | One c <- toList ones ] ++ "]"
      resstr = drop 1 (concat [ '|' : showRE r | r <- S.toList res ])
      str = case (S.null ones, S.null res) of
         (_,     True)  -> onestr
         (True,  False) -> "(" ++ resstr ++ ")"
         (False, False) -> "(" ++ resstr ++ "|" ++ onestr ++ ")"
  in str
showRE (Star re) = "(" ++ showRE re ++ ")*"
printRE = putStrLn . showRE
