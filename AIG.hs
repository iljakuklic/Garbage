
{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

import Prelude hiding (concat, and)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.Trans.State
import Data.Traversable(Traversable, traverse, fmapDefault, foldMapDefault)
import qualified Data.Foldable as F
import Data.Functor.Identity
import Control.Applicative
import qualified System.Process as P

-- TODO protect build data structure using existentials?

newtype Fix f = Fix { unFix :: f (Fix f) }
newtype NID = NID Int deriving (Eq, Ord)
instance (Show NID) where show (NID n) = '@' : show n

data Node f = Node { getID :: NID, getNode :: f (Node f) }

showNode :: (Functor f, Show (f NID)) => Node f -> String
showNode (Node i n) = show i ++ " => " ++ show (nodeRep n)

type Visited = Bool

nfold :: (Traversable f) => (NID -> f (Visited, a) -> a) -> Node f -> a
nfold f n = snd $ evalState (nfoldS f n) M.empty
nfoldS f (Node nid node) = do
  cache <- fmap (M.lookup nid) get
  case cache of
    Just cc -> return (True, cc)
    Nothing -> do
      res <- fmap (f nid) $ traverse (nfoldS f) node
      modify (M.insert nid res)
      return (False, res)


instance Eq (Node f) where
  (Node nid1 _) == (Node nid2 _) = nid1 == nid2
instance Ord (Node f) where
  compare (Node nid1 _) (Node nid2 _) = compare nid1 nid2

unobserve :: Functor f => Node f -> Fix f
unobserve = Fix . fmap unobserve . getNode

nodeRep :: (Functor f) => f (Node f) -> f NID
nodeRep = fmap getID
graphRep :: (Traversable f) => Node f -> [(NID, f NID)]
graphRep = nfold (\i n -> (i, fmap (fst.head.snd) n) : prev i n)
  where prev i n = F.concat [ n' | (False, n') <- F.toList n]

showGraph nn = drop 2 $ F.concat [ "; " ++ show i ++ " => " ++ show n | (i, n) <- graphRep nn]

data BuilderState f = BldSte { nextNID :: Int, exprMap :: M.Map (f NID) (Node f) }

newtype BuilderT f m a = BldT { runBldT :: StateT (BuilderState f) m a }

instance (Monad m) => Monad (BuilderT f m) where
  return = BldT . return
  (BldT s1) >>= f = BldT (s1 >>= runBldT . f)
instance (Monad m) => Functor (BuilderT f m) where
  fmap f x = x >>= return . f
instance (Monad m) => Applicative (BuilderT f m) where
  (<*>) = ap
  pure = return
--TODO
--instance MonadTrans (BuilderT s) where
--  lift (BldT s) = lift s

runBuilderT :: Monad m => BuilderT f m a -> m a
runBuilderT = flip evalStateT (BldSte 0 M.empty) . runBldT
runBuilder = runIdentity . runBuilderT

node :: (Monad m, Functor f, Ord (f NID)) => f (Node f) -> BuilderT f m (Node f)
node nod = do
  BldSte freshID exprs <- BldT get
  let rep = nodeRep nod
  case M.lookup rep exprs of
    Just x -> return x
    Nothing -> do
      let newNode = Node (NID freshID) nod
      BldT . put $ BldSte (freshID + 1) (M.insert rep newNode exprs)
      return newNode

rebuild :: (Monad m, Traversable f, Ord (f NID)) => (Fix f) -> BuilderT f m (Node f)
rebuild (Fix x) = traverse rebuild x >>= node

data BrType = Pos | Neg deriving (Eq, Show, Ord)
data Branch e = Branch BrType e deriving (Eq, Ord)
data AIGF v e = One | Var v | And (Branch e) (Branch e) deriving (Eq, Ord)

type AIGB v = Branch (Node (AIGF v))
type AIGM m v = BuilderT (AIGF v) m (AIGB v)

instance Show e => Show (Branch e) where
  show (Branch t e) = showBrT t ++ show e
    where
      showBrT Pos = ""
      showBrT Neg = "¬"
instance (Show v, Show e) => Show (AIGF v e) where
  show One = "T"
  show (Var v) = show v
  show (And a b) = "(" ++ show a ++ " & " ++ show b ++ ")"

instance Functor Branch where
  fmap = fmapDefault
instance F.Foldable Branch where
  foldMap = foldMapDefault
instance Traversable Branch where
  traverse f (Branch t x) = Branch t <$> f x
instance Functor (AIGF v) where
  fmap = fmapDefault
instance F.Foldable (AIGF v) where
  foldMap = foldMapDefault
instance Traversable (AIGF v) where
  traverse f One = pure One
  traverse f (Var v) = pure $ Var v
  traverse f (And e1 e2) = And <$> traverse f e1 <*> traverse f e2

true, false :: (Monad m, Ord v) => AIGM m v
true  = Branch Pos <$> node One
false = Branch Neg <$> node One
isOne ty (Branch t (Node _ n)) = n == One && t == ty
var :: (Monad m, Ord v) => v -> AIGM m v
var = fmap (Branch Pos) . node . Var
mkAnd :: (Monad m, Ord v) => AIGB v -> AIGB v -> AIGM m v
mkAnd b1@(Branch t1 n1) b2@(Branch t2 n2)
  | b1 > b2 = mkAnd b2 b1
  | t1 == t2 && n1 == n2 = return b1
  | t1 /= t2 && n1 == n2 = false
  | isOne Neg b1 || isOne Neg b2 = false
  | isOne Pos b1 = return b2
  | isOne Pos b2 = return b1
  | otherwise = Branch Pos <$> node (And b1 b2)
mkAIGM (Branch Pos a) = return a
mkAIGM b@(Branch Neg _) = true >>= \t -> node $ And b t
mkAIG x = runBuilder (true >> x >>= mkAIGM)

neg = fmap $ \(Branch t b) -> Branch (case t of Pos -> Neg; Neg -> Pos) b

x .& y = do x' <- x; y' <- y; mkAnd x' y'
x .| y = neg (neg x .& neg y)
x .= y = (x .-> y) .& (y .-> x)
x .^ y = (x .-> neg y) .& (y .-> neg x)
x .-> y = neg x .| y
c .? (t, f) = (c .& t) .| (neg c .& f)

displayDot = P.readProcess "dot" ["-Tx11"] . dotRep . graphRep
  where
    dotRep g = wrap (fmap node g ++ [""] ++ edges g)
    wrap lns = "digraph {\n" ++ unlines (fmap ("    " ++) lns) ++ "}\n"
    node (i, aig) = shID i ++ " [" ++ shAig aig ++ "]"
    shAig One = "shape=diamond,label=\"\""
    shAig (And _ _) = "shape=circle,label=\"\""
    shAig (Var x) = "shape=box,label=" ++ show (show x)
    edges g = F.concat [ [ edge n1 f t1 , edge n2 f t2 ] |
        (f, And (Branch n1 t1) (Branch n2 t2)) <- g ]
    edge n f t = shID f ++ " -> " ++ shID t ++ " [style=" ++ edgeSty n ++ "]"
    edgeSty t = case t of Pos -> "solid"; Neg -> "dashed"
    shID i = show ('n' : show i)

infixr 1 .->, .=, .?
infixr 2 .|
infixr 3 .&

