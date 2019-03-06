{-# LANGUAGE NoMonomorphismRestriction, LambdaCase, OverloadedStrings,
             FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving,
             TupleSections, OverloadedLists, TypeFamilies #-}

import Data.Monoid
import Data.Text as T
import Control.Applicative
import Control.Monad.Writer
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Exts(IsString(..), IsList(..))

newtype LS = LS [Text] deriving (Eq, Ord, Show, Monoid)
getLS (LS x) = x
mkLS = LS . pure

instance Num LS where
  (+) = (<>)
  LS as * LS bs = LS (liftA2 mappend as bs)
  abs = id
  negate = error "Num sucks"
  signum = \case LS [] -> 0; _ -> 1
  fromInteger = LS . flip L.genericReplicate mempty

instance IsString LS where
  fromString = mkLS . T.pack
instance IsList LS where
  type Item LS = Text
  fromList = LS
  toList = getLS

newtype Name = Name Text deriving (Eq, Ord, Show)
getName (Name n) = n

type Env = M.Map Name LS

expand :: Env -> LS -> (S.Set Name, LS)
expand env = foldMap get . getLS
  where get n = maybe (err n) (mempty,) (M.lookup (Name n) env)
        err n = (S.singleton (Name n), mempty)

data StrPat = StrPat {
    strPatInit :: Text,
    strPatBind :: [(Name, Text)]
  } deriving (Eq, Show)
type SeqPat = [StrPat]
type AltPat = [SeqPat]

bindP s = StrPat "" [(Name s, "")]

instance Monoid StrPat where
  mempty = StrPat "" []
  mappend (StrPat s1 []) (StrPat s2 bs) = StrPat (s1 <> s2) bs
  mappend (StrPat s1 [(n, s2)]) (StrPat s3 bs) = StrPat s1 ((n, s2 <> s3):bs)
  mappend (StrPat s1 (b1:bs1)) rhs =
    let StrPat "" bs2 = StrPat "" bs1 <> rhs in StrPat s1 (b1:bs2)

instance IsString StrPat where
  fromString = flip StrPat [] . T.pack

matchStr :: StrPat -> Text -> Maybe Env
matchStr (StrPat pre bs) str = T.stripPrefix pre str >>= matchStr1 bs
matchStr1 :: [(Name, Text)] -> Text -> Maybe Env
matchStr1 [] "" = Just mempty
matchStr1 [] _ = Nothing
matchStr1 [(n,s)] str = fmap (M.singleton n . mkLS) (T.stripSuffix s str)
matchStr1 ((n,s):ps) str = do
  let (val, restPre) = T.breakOn s str
  rest <- T.stripPrefix s restPre
  fmap (M.insertWith (<>) n (mkLS val)) (matchStr1 ps rest)

matchSeq :: SeqPat -> LS -> Maybe (Env, LS)
matchSeq [] rest = Just (mempty, rest)
matchSeq (p:ps) (LS (s:ss)) = do
  e <- matchStr p s
  (es, rest) <- matchSeq ps (LS ss)
  return (M.unionWith mappend e es, rest)

matchAlt :: AltPat -> LS -> Maybe (Env, LS)
matchAlt ps ss = L.foldr (\p r -> matchSeq p ss <|> r) Nothing ps
