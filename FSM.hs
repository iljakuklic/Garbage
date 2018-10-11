{-# LANGUAGE ConstraintKinds #-}

import Data.Array
import Data.Set as S hiding (map, toList)
import Data.Monoid
import Data.Foldable
import Data.List (nub)

newtype TF s = TF { getTF :: Array s s }
  deriving (Eq, Ord, Show)
type Ste s = (Bounded s, Enum s, Ix s)

mkTF :: Ste s => [s] -> TF s
mkTF tf = TF $ array (minBound, maxBound) (zip [minBound..maxBound] tf)

instance Ste s => Monoid (TF s) where
  mempty = mkTF [minBound..maxBound]
  mappend (TF a) (TF b) = TF (fmap (b!) a)

eqfix f = \x -> let fx = f x in if fx == x then x else eqfix f fx


data S = A | B | C | D deriving (Eq, Ord, Bounded, Enum, Ix, Show)
ex1 = mkTF [A, C, C, B]
ex2 = mkTF [A, D, B, A]

closure :: (Ord m, Monoid m) => Set m -> Set m
closure g = eqfix
            (\t -> fromList (zipWith (<>) (toList g) (toList t)) <> t <> g)
            (S.singleton mempty)

data Str = Out | Num | Frc | Str | Esc | Cmt | Bad
     deriving (Eq, Ord, Bounded, Enum, Ix, Show)

--               Out, Num, Frc, Str, Esc, Cmt, Bad
strChar = mkTF [ Bad, Bad, Bad, Str, Bad, Cmt, Bad ]
strUS   = mkTF [ Bad, Out, Out, Str, Str, Cmt, Bad ]
strTN   = mkTF [ Bad, Bad, Bad, Str, Str, Cmt, Bad ]
strQuot = mkTF [ Str, Bad, Bad, Out, Str, Cmt, Bad ]
strBs   = mkTF [ Bad, Bad, Bad, Esc, Str, Cmt, Bad ]
strSpc  = mkTF [ Out, Out, Out, Str, Bad, Cmt, Bad ]
strNum  = mkTF [ Num, Num, Frc, Str, Str, Cmt, Bad ]
strHash = mkTF [ Cmt, Cmt, Bad, Str, Bad, Cmt, Bad ]
strNL   = mkTF [ Out, Out, Out, Bad, Bad, Out, Bad ]
strDot  = mkTF [ Bad, Frc, Bad, Str, Bad, Cmt, Bad ]

strTFs = closure (fromList [strChar, strQuot, strBs, strSpc, strTN, strNum, strUS, strHash, strNL, strDot])

printTFs :: Show s => Foldable t => t (TF s) -> IO ()
printTFs = mapM_ (print . toList . getTF)
