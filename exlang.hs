{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import           Bound
import           Control.Applicative
import           Control.Monad hiding (traverse, forM, forM_, sequence)
import           Data.Ord (comparing)
import           Data.List (sort, sortBy)
import           Data.String (IsString, fromString)
import           Data.Foldable
import           Data.Traversable
import           GHC.Show
import           Prelude hiding (lex, foldl, elem, sequence)
import           Prelude.Extras (Show1, Eq1)
import qualified Text.Parsec       as P
import qualified Text.Parsec.Token as P

-- ABSTRACT SYNTAX TREE

data Term a = Star
			| Var a
			| Symbol String
			| SEnum [String]
			| Case [(String, Term a)]
			| Term a ::: Term a
			| Term a :=> Scope () Term a
			| Term a :@: Term a
			| Lam (Scope () Term a)
			-- TODO: Enums, W-types
			deriving (Show, Functor, Foldable, Traversable)

-- classes & substitution

instance Eq a => Eq (Term a) where
	Star == Star = True
	Symbol s1 == Symbol s2 = s1 == s2
	SEnum ss1 == SEnum ss2 = sort ss1 == sort ss2
	Case cas1 == Case cas2 = sort (map fst cas1) == sort (map fst cas2) -- BUG!!
	(e1:::t1) == (e2:::t2) = e1 == e2 && t1 == t2
	(d1:=>c1) == (d2:=>c2) = d1 == d2 && c1 == c2
	(f1:@:x1) == (f2:@:x2) = f1 == f2 && x1 == x2
	Lam f1    == Lam f2    = f1 == f2
	Var v1    == Var v2    = v1 == v2
	_         == _         = False
instance Eq1 Term where
instance Show1 Term where
instance Monad Term where
	return = Var
	Star          >>= f = Star
	Symbol s      >>= f = Symbol s
	SEnum ss      >>= f = SEnum ss
	Case cs       >>= f = Case $ fmap (\(s,c) -> (s, c >>= f)) cs
	Var x         >>= f = f x
	(term ::: ty) >>= f = (term >>= f) ::: (ty >>= f)
	(arg :=> res) >>= f = (arg >>= f) :=> (res >>>= f)
	(fun :@: arg) >>= f = (fun >>= f) :@: (arg >>= f)
	Lam fun       >>= f = Lam (fun >>>= f)

-- EXPRESSION EVALUATION

type Result a = Either String a

eval :: (Show a) => Term a -> Result (Term a)
eval (term ::: ty) = eval term
eval term@(fun :@: arg) = eval fun >>= \case
	Lam body -> (flip instantiate1 body <$> eval arg) >>= eval
	Case cs  -> eval arg >>= \case
		sym@(Symbol s) -> case lookup s cs of
			Just body -> eval body
			Nothing   -> err ("Symbol not handled: " ++ s)
		x -> err ("Symbol expected: " ++ pretty x)
	x -> err ("Function expected: " ++ pretty x)
  where err = Left . showString ("FATAL error: evaluating " ++ pretty term ++ "\n")
eval x = Right x
evalIn env term = resolve env term >>= eval

-- TYPE CHECKING

hasTy fv term ty = checkTy fv term (Just ty)
inferTy fv term = checkTy fv term Nothing
typeFamily ix = ix :=> abstract (const Nothing) Star

data TcVar = Abstract Int | Known (Term TcVar) deriving (Eq, Show)
type TcTerm = Term TcVar

checkTy :: Int -> TcTerm -> Maybe TcTerm -> Result TcTerm
checkTy fv Star Nothing = return Star
checkTy fv v@(Var (Known t)) Nothing = return t
checkTy fv (Var (Abstract v)) Nothing = Left "FIXME"
checkTy fv (SEnum ss) Nothing = return Star
checkTy fv (term ::: ty) Nothing = hasTy fv ty Star >> eval ty >>= hasTy fv term
checkTy fv (fun :@: arg) Nothing = inferTy fv fun >>= \case
	argTy :=> resTy -> do
		hasTy fv arg argTy
		arg' <- eval arg
		eval (instantiate1 arg' resTy)
	wrongTy -> Left ("argument applied to a non-function: " ++ pretty wrongTy)
checkTy fv (argTy :=> resTy) Nothing = do
	hasTy fv argTy Star
	argTy' <- eval argTy
	hasTy fv (instantiate1 (Var $ Known argTy') resTy) Star
	return Star
checkTy fv c@(Case cases) Nothing = case hasTy fv c (typeFamily (SEnum $ map fst cases)) of
		r@(Right _) -> r
		Left _ -> Left ("Unable to infer type, provide signature for: " ++ pretty c)
checkTy fv term Nothing = Left ("Unable to infer type, provide signature for: " ++ pretty term)
checkTy fv (Lam body) (Just ty@(argTy :=> resTy)) = do
	let fv' = succ fv
	    resTy' = instantiate1 (Var $ Abstract fv) resTy
	    body' = instantiate1 (Var $ Known argTy) body
	hasTy fv' body' resTy'
	return ty
checkTy fv (Case cases) (Just ty@(argTy@(SEnum es) :=> resTy)) = do
	let csyms = map fst cases
	when (sort csyms /= sort es) $ Left $
		"Case expression symbols " ++ show csyms ++ " do not match the type " ++ pretty argTy
	forM_ cases $ \(sym, body) -> eval (instantiate1 (Symbol sym) resTy) >>= hasTy fv body
	return ty
checkTy fv sym@(Symbol s) (Just ty@(SEnum ss)) = do
	if s `elem` ss
		then return ty
		else Left $ "Symbol " ++ pretty sym ++ " not in set " ++ pretty ty
checkTy fv term (Just ty) = do
	ty' <- inferTy fv term
	when (ty /= ty') $ Left $
		"Type mismatch in:   " ++ pretty term ++ "\n" ++
		"    Expected:       " ++ pretty ty ++ "\n" ++
		"    Expression has: " ++ pretty ty'
	return ty
checkDecls' :: [(String,TcTerm)] -> [(String,Term String)] -> Result [(String,TcTerm)]
checkDecls' env [] = return env
checkDecls' env ((name,term):ds) = do
	when (name `elem` map fst env) $ Left ("Multiple declarations of " ++ show name)
	term' <- resolve env term
	inferTy 0 term'
	checkDecls' ((name,term'):env) ds
checkDecls = fmap reverse . checkDecls' []
resolve env term = do
	let resolver n = maybe (Left $ "Unknown identifier " ++ show n) return $ lookup n env
	term' <- fmap join $ traverse resolver term
	maybe (Left $ "Term not fully resolved: " ++ pretty term') return (closed term')

-- PRETTY PRINTING

data Prio = PrioAnot | PrioLam | PrioFun | PrioApp | PrioMax
	deriving (Eq, Ord, Enum, Show, Bounded)

prettyT :: Int -> Prio -> Term ShowS -> ShowS
prettyT bx prio Star = showString "U"
prettyT bx prio (Var x) = x
prettyT bx prio (Symbol s) = showChar '.' . showString s
prettyT bx prio (SEnum ss) = showString ".{ " .
	foldl (.) id (map (showString . (++ " ")) ss) . showChar '}'
prettyT bx prio (Case cs) = showString "{ " . foldl (.) id (map pretty1 cs) . showChar '}'
  where pretty1 (s, b) = showString (s ++ ": ") . prettyT bx minBound b . showString "; "
prettyT bx prio (term ::: ty) = showParen (prio > PrioAnot) $
    prettyT bx (succ PrioAnot) term . showString " : " . prettyT bx (succ PrioAnot) ty
prettyT bx prio (fun :@: arg) = showParen (prio > PrioApp) $
    prettyT bx PrioApp fun . showChar ' ' . prettyT bx (succ PrioApp) arg
prettyT bx prio (dom :=> res) = showParen (prio > PrioFun) $
    prettyT bx (succ PrioFun) dom . showString " => " . prettyS bx PrioFun res
prettyT bx prio (Lam body) = prettyS bx prio body
prettyS bx prio scope = let x = showChar 'x' . shows bx in
    showString "[" . x . showString "] " . prettyT (succ bx) prio (instantiate1 (return x) scope)
pretty term = prettyT 0 minBound (fmap shows term) ""

-- LEXER

opChars = "|!@#$%^&*+~/-=:<>?" :: String
lexerDef = P.LanguageDef {
    P.commentStart    = "{-",
    P.commentEnd      = "-}",
	P.commentLine     = "--",
	P.nestedComments  = True,
	P.identStart      = P.letter <|> P.oneOf "_",
	P.identLetter     = P.alphaNum <|> P.oneOf "_'",
	P.opStart         = P.oneOf opChars,
	P.opLetter        = P.oneOf opChars,
	P.caseSensitive   = True,
	P.reservedNames   = [ "U", "let" ],
	P.reservedOpNames = [ "=", "|", ",", "->", "=>", "<-", "<=", "*" ]
  }

compiledLexerDef = P.makeTokenParser lexerDef
lex f = f compiledLexerDef

-- PARSER

pIdents = many (lex P.identifier)
pBinders f sub = abstr <$> (reverse <$> lex P.brackets pIdents) <*> sub
  where abstr = flip $ foldl (\t v -> f $ abstract1 (case v of "_" -> ""; _ -> v) t)
pBinder sub = abstract1 <$> lex P.brackets (lex P.identifier) <*> sub
pBinderOpt sub = pBinder sub <|> abstract1 "" <$> sub

pExpr = pAnot <|> pLam
pAnot = pArr >>= \a -> (a :::) <$> (lex P.reservedOp ":" *> pArr) <|> return a
pLam  = pBinders Lam pExpr
pStar = Star <$ lex P.reserved "U"
pVar  = Var <$> lex P.identifier
pAtom = pStar <|> pVar <|> pEnum <|> pCase <|> lex P.parens pExpr
pEnum = P.char '.' *> (Symbol <$> lex P.identifier <|>
	SEnum <$> lex P.braces (many $ lex P.identifier))
pCase = Case <$> lex P.braces (pCase1 `P.sepEndBy` lex P.semi)
pCase1 = (,) <$> lex P.identifier <* lex P.colon <*> pExpr
pArr = (pApp >>= \l -> (fmap (l :=>) rgt <|> return l)) <|> ((:=>) <$> pApp <*> rgt)
  where rgt = lex P.reservedOp "=>" *> pBinderOpt pExpr
pApp = pAtom >>= pApp'
pApp' lft = (pAtom >>= pApp' . (lft :@:)) <|> return lft
pTopE = lex P.whiteSpace *> pExpr <* P.eof >>= maybe (fail "Unresolved variables") return . closed
pDecl = (,) <$> lex P.identifier <*> (lex P.reservedOp "=" *> pExpr <|>
	flip (:::) <$ lex P.reservedOp ":" <*> pExpr <* lex P.reservedOp "=" <*> pExpr)
pDecls = lex P.whiteSpace *> (pDecl `P.sepEndBy` lex P.semi) <* P.eof

-- DRIVER

doParse p nme str = either (Left . show) Right (P.runParser p () nme str)
runResult = either error return
load fname = readFile fname >>= \str -> runResult (doParse pDecls fname str >>= checkDecls)
run :: [(String, TcTerm)] -> String -> IO ()
run env expr = runResult proc >>= putStr
	where proc = do
		e' <- doParse pExpr "IN" expr >>= resolve env
		let e = e'
		t <- inferTy 0 e
		r <- eval e -- :: Result (Term String)
		return $ unlines [pretty t, pretty r]
