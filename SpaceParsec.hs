{-# LANGUAGE NoMonomorphismRestriction,
             FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances,
             StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module SpaceParsec(
    SpaceParsecT, SpaceParsec, runParser,
    lexeme, symbol, symbol', getSpaceParser, withSpace,
    indentLevel, indentGuard, nonIndented, indentBlock
  ) where

import Control.Monad.Reader
import Data.Functor.Identity
import Control.Applicative

import qualified Text.Megaparsec       as P
import qualified Text.Megaparsec.Lexer as P
import qualified Text.Megaparsec.Prim  as P

type SpaceParsecImpl s m a = ReaderT (SpaceParsecT s m ()) (P.ParsecT s m) a
newtype SpaceParsecT s m a = SPT { runSPT :: SpaceParsecImpl s m a }
type SpaceParsec s a = SpaceParsecT s Identity a

deriving instance Functor (SpaceParsecT s m)
deriving instance Applicative (SpaceParsecT s m)
deriving instance Monad (SpaceParsecT s m)
deriving instance Alternative (SpaceParsecT s m)
deriving instance MonadPlus (SpaceParsecT s m)

inSPT f = SPT . f . runSPT

instance P.Stream s t => P.MonadParsec s (SpaceParsecT s m) t where
  failure msgs = SPT (P.failure msgs)
  label s = inSPT (P.label s)
  try = inSPT P.try
  lookAhead = inSPT P.lookAhead
  eof = SPT P.eof
  notFollowedBy = inSPT P.notFollowedBy
  token adv = SPT . P.token adv
  tokens adv cond = SPT . P.tokens adv cond
  getParserState = SPT P.getParserState
  updateParserState = SPT . P.updateParserState

instance MonadTrans (SpaceParsecT s) where
  lift = SPT . lift . lift

instance MonadIO m => MonadIO (SpaceParsecT s m) where
  liftIO = SPT . liftIO

liftWS :: (SpaceParsecImpl s m () -> a -> SpaceParsecImpl s m b) -> a -> SpaceParsecT s m b
liftWS act arg = SPT $ do
  SPT ws <- ask
  act ws arg

-- lexeme, symbol
lexeme  = liftWS P.lexeme . runSPT
symbol  = liftWS P.symbol
symbol' = liftWS P.symbol'

getSpaceParser = SPT ask
withSpace spc = inSPT $ local (const spc)

-- run the parser
runParser (SPT p) spc = P.runParser (runReaderT p spc)
-- TODO other variants?

-- indentation-sensitive parsing combinators
indentLevel = SPT P.indentLevel
indentGuard = liftWS P.indentGuard
nonIndented = liftWS P.nonIndented
indentBlock = liftWS P.indentBlock
