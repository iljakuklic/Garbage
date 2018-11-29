
{-# LANGUAGE RankNTypes #-}

import qualified Data.Map as M

newtype Symbol = Symbol String deriving (Eq, Ord)
instance Show Symbol where show (Symbol s) = s

data TermI p = Var p                        -- variable
             | Star                         -- Type kind
             | TermC p :! TermC p           -- type annotation
             | Free Symbol                  -- Free var
             | TermC p :~> (p -> TermC p)   -- dependent function type
             | TermI p :$ TermC p           -- function elimination (application)

data TermC p = Lam (p -> TermC p)           -- function introduction (λ-abstraction)
             | Inf (TermI p)                -- make inferable term

newtype Term = Term { unTerm :: forall p . TermI p }

type Context = M.Map Symbol Term
type Result a = Either String a

err = Left

lookupCtx :: Symbol -> Context -> Result Term
lookupCtx nme env = maybe (err $ "No such symbol: " ++ show nme) Right (M.lookup nme env)

check :: Term -> Result ()
check ctx = checkI . unTerm
  where
    checkC :: TermC p -> TermC p -> Result ()
    checkC (Inf t) ty = do
      ty' <- checkI t
      if ty' == ty then return () else err "Type mismatch"
    checkC (Lam f) ty = case ty of
      Inf (tyA :~> tfB) -> do
        checkC (f tyA) (tfB tyA)
      _ -> err "Function type expected"
    checkI = undefined

whnf = id
