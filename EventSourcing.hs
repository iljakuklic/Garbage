{-# LANGUAGE NoMonomorphismRestriction, DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, RankNTypes, GADTs, TypeOperators #-}


import Prelude hiding (foldl)
import Data.Monoid
import Data.Foldable as F
import Data.Traversable
import Control.Applicative
import qualified Data.Map as M
import Data.Profunctor

type UserID = Int
type PostID = Int

data Event = UserRegistered UserID String
           | PostAdded PostID UserID String String
	         | PostRemoved PostID
           | PostTitleChanged PostID String
           | PostContentChanged PostID String
           deriving Show

type Log = [Event]

countUsers :: Log -> Int
countUsers = foldl updateCounts 0
  where
    updateCounts num (UserRegistered _ _) = num + 1
    updateCounts num _                    = num

countPosts :: Log -> Int
countPosts = foldl (+) 0 . map countFor
  where
    countFor (PostAdded _ _ _ _) =  1
    countFor (PostRemoved _)     = -1
    countFor _                   =  0

{-
newtype Log a = Log [a] deriving (Functor, Foldable, Traversable)

instance Show a => Show (Log a) where
	show (Log xs) = unlines . map show $ xs

instance Monoid (Log a) where
	mempty = Log mempty
	mappend (Log xs) (Log ys) = Log (xs <> ys)
-}

newtype Map k v = Map (M.Map k v) deriving (Functor, Foldable, Traversable, Show)

instance (Ord k, Monoid v) => Monoid (Map k v) where
	mempty = Map mempty
	mappend (Map x) (Map y) = Map (M.unionWith mappend x y)
	mconcat ms = Map $ M.unionsWith mappend [ m | Map m <- ms ]

singleton k v = Map (M.singleton k v)

data CUD p a = Create a | Update p | Delete deriving (Functor, Show)

class Monoid p => Updatable p a | p -> a where
	patch :: p -> a -> a
	-- LAWS: patch mempty = id, patch p . patch q = patch (p <> q)
	-- (is patch a monoid homomorphism from p to Endo a?)

instance (Updatable p a, Monoid p) => Monoid (CUD p a) where
	mempty = Update mempty
	mappend _            Delete     = Delete
	mappend _         c@(Create _) = c
	mappend (Update p)  (Update q)  = Update (p <> q)
	mappend (Create c)  (Update p)  = Create (patch p c)
	mappend Delete      (Update _)  = Delete

users :: Event -> (Map Int String)
users (UserRegistered uid name) = singleton uid name
users _ = mempty

newtype Post = Post (Int, String, String) deriving (Show)
newtype PostPatch = PostPatch (Last String, Last String) deriving (Show)

title (Post (_, t, _)) = t

instance Monoid PostPatch where
	mempty = PostPatch mempty
	mappend (PostPatch p) (PostPatch q) = PostPatch (p <> q)

instance Updatable (Last a) a where
	patch (Last Nothing) x = x
	patch (Last (Just x)) _ = x

instance Updatable PostPatch Post where
	patch (PostPatch (ptitle, ptext)) (Post (pid, title, text)) =
		Post (pid, patch ptitle title, patch ptext text)

type PostVal = CUD PostPatch Post
posts :: Event -> (Map Int PostVal)
posts (PostAdded pid uid title text) = singleton pid . Create . Post $ (uid, title, text)
posts (PostTitleChanged pid title) = singleton pid . Update . PostPatch $ (lj title, ln)
posts (PostRemoved pid) = singleton pid Delete
posts _ = mempty
lj = Last . Just
ln = Last Nothing
postsAndUsers = (,) <$> posts <*> users

log1 = [
	UserRegistered 1 "Franta",
	PostAdded 1 1 "Test Post" "testing...",
	PostAdded 2 1 "Franta's post" "foo bar baz, blah blah",
	PostRemoved 1,
	PostTitleChanged 2 "Franta's cool post",
	PostTitleChanged 1 "Invalid"
  ]

{-
splitLog n (Log l) = let (l1, l2) = splitAt n l in (Log l1, Log l2)
(log1A, log1B) = splitLog 4 log1
-}

data Hidden f a b = forall h . Hidden (a -> h) (h -> b) (f h)
instance Functor (Hidden f a) where
	fmap = rmap
instance Profunctor (Hidden f) where
	dimap f g (Hidden a b h) = Hidden (a . f) (g . b) h
instance Applicative f => Applicative (Hidden f a) where
	pure x = Hidden (const ()) (const x) (pure ())
	Hidden a1 b1 h1 <*> Hidden a2 b2 h2 = Hidden
		(\e -> (a1 e, a2 e))
		(\(f, x) -> b1 f (b2 x))
		(liftA2 (,) h1 h2)  -- can we do without liftA2 here?

patchPost (pid, title, text) (title', text') = (pid, maybe title id title', maybe text id text')

data MQuery e a = forall m . MQuery (e -> m) (m -> a) (m -> m -> m) m

mquery :: Foldable t => MQuery e r -> t e -> r
mquery (MQuery em ma mm n) = ma . F.foldl' (\m e -> mm m (em e)) n

qmonoid = MQuery id id
qmon = qmonoid mappend mempty
qgroup :: (Ord k) => (e -> k) -> MQuery e r -> MQuery e (M.Map k r)
qgroup kf (MQuery em ma mm n) = MQuery (\e -> M.singleton (kf e) (em e))
	(fmap ma) (M.unionWith mm) M.empty
qnewmonoid con decon = MQuery con decon mappend mempty
qsum = qmonoid (+) 0
qproduct = qmonoid (*) 1
qcount = MQuery (const 1) id (+) 0
qlast = qnewmonoid (Last . Just) getLast
qfirst = qdual qlast
qlist = MQuery pure id (++) []
qalt = MQuery pure id (<|>) empty
qhaving :: (e -> Bool) -> MQuery e r -> MQuery e r
qhaving p (MQuery em ma mm n) = MQuery (\e -> if p e then em e else n) ma mm n
qfilert = qhaving
qdual (MQuery em ma mm n) = MQuery em ma (flip mm) n
qany = qmonoid (||) False
qall = qmonoid (&&) True
-- !!! abstraction leak ahead !!! patch neads to be a monoid homomorphism from p to Endo t
qcud :: Monoid p => (p -> t -> t) -> MQuery (CUD p t) (CUD p t)
qcud patch = qmonoid mm (Update mempty) where
	mm _          (Delete)   = Delete
	mm _          (Create c) = Create c
	mm (Update p) (Update q) = Update (p <> q)
	mm (Create c) (Update p) = Create (patch p c)
	mm (Delete)   (Update _) = Delete
qmaximum = MQuery Just id mm Nothing where
	mm Nothing y = y
	mm x Nothing = x
	mm (Just x) (Just y) = Just (max x y)
on :: MQuery e' r -> (e -> e') -> MQuery e r
on (MQuery em ma mm n) f = MQuery (em . f) ma mm n
(@@) = on
infixl 8 @@


instance Functor (MQuery e) where
	fmap f (MQuery em ma mm n) = MQuery em (f . ma) mm n
instance Applicative (MQuery e) where
	pure x = MQuery (const ()) (const x) (<>) ()
	MQuery em1 ma1 mm1 n1 <*> MQuery em2 ma2 mm2 n2 = MQuery
		(\e -> (em1 e, em2 e))
		(\(f, x) -> ma1 f (ma2 x))
		(\(a1, a2) (b1, b2) -> (mm1 a1 b1, mm2 a2 b2))
		(n1, n2)

instance Num r => Num (MQuery e r) where
	(+) = liftA2 (+)
	(*) = liftA2 (*)
	abs = liftA abs
	signum = liftA signum
	fromInteger = pure . fromInteger
instance Fractional r => Fractional (MQuery e r) where
	recip = liftA recip
	fromRational = pure . fromRational

qstats = go <$> qcount <*> qsum <*> (qsum @@ (^2))
	where
		go s0 s1 s2 = let
			n    = fromIntegral s0
			mean = s1 / n
			std  = sqrt (s2 / n - mean^2)
			in (n, mean, std)
titleLength = fromIntegral . length . title
