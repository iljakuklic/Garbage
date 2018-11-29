import Data.Foldable
import Data.Monoid

data Parens = Parens String String | Mismatch deriving (Eq, Show)

lparen c = Parens "" c
rparen c = Parens c ""

parseChar '(' = lparen "("
parseChar ')' = rparen "("
parseChar '[' = lparen "["
parseChar ']' = rparen "["
parseChar '{' = lparen "{"
parseChar '}' = rparen "{"
parseChar _ = mempty

collapse :: String -> String -> Maybe (Either String String)
collapse xs "" = Just (Left xs)
collapse "" ys = Just (Right ys)
collapse (x:xs) (y:ys) | x == y = collapse xs ys
collapse _ _ = Nothing

instance Monoid Parens where
  mempty = Parens "" ""

  mappend _ Mismatch = Mismatch
  mappend Mismatch _ = Mismatch
  mappend (Parens s1 s2) (Parens s3 s4) =
    case collapse s2 s3 of
      Nothing -> Mismatch
      Just (Left xs) -> Parens s1 (s4 ++ xs)
      Just (Right xs) -> Parens (s1 ++ xs) s4

parse :: String -> Parens
parse = foldMap parseChar

isDyck str = parse str == mempty

prop_parens_left_identity a = mempty <> a == a
prop_parens_right_identity a = a <> mempty == a
prop_parens_assoc a b c = (a <> b) <> c == a <> (b <> c)
prop_parse_homo xs ys = (parse xs <> parse ys) == parse (xs ++ ys)

prop_add_comm a b = a + b == b + a

-- Parsing numeric literals monoidally.

data NumLit a = NumLit a a deriving (Eq, Show)
numLit x = NumLit 1 x

instance Integral a => Monoid (NumLit a) where
  mempty = NumLit 0 0
  mappend (NumLit e1 x1) (NumLit e2 x2) = NumLit (e1 + e2) (act x1 e2 + x2)
    where act x e = x * 10^e

parseLitChar :: Char -> NumLit Integer
parseLitChar = numLit . read . pure
parseLit :: String -> NumLit Integer
parseLit = foldMap parseLitChar

-- File path monoid

data AddSet m = Add m | Set m deriving (Eq, Ord, Show)
instance Monoid m => Monoid (AddSet m) where
  mempty = Add mempty
  mappend _ y@(Set _) = y
  mappend (Set x) (Add y) = Set (x <> y)
  mappend (Add x) (Add y) = Add (x <> y)

parsePath "" = []
parsePath ('/':p) = parsePath p
parsePath p = let (c, cs) = span (/= '/') p in c : parsePath cs
showPathComps cs = concatMap (\c -> '/':c) cs
showPath (Add cs) = "path " ++ show (drop 1 (showPathComps cs))
showPath (Set cs) = "path " ++ show (showPathComps cs)
path p = (if take 1 p == "/" then Set else Add) (parsePath p)

-- Sum (prioritized) monoid
-- Left has low priority, Right has high priority
instance (Monoid l, Monoid r) => Monoid (Either l r) where
  mempty = Left mempty
  mappend (Left a) (Left b) = Left (a <> b)
  mappend (Left _) b@(Right _) = b
  mappend a@(Right _) (Left _) = a
  mappend (Right a) (Right b) = Right (a <> b)

-- Layout monoid

data Layout = Layout { lHeight :: Int, lWidth :: Int, lLastWidth :: Int }
              deriving (Eq, Show)

instance Monoid Layout where
  mempty = Layout 0 0 0
  mappend (Layout h1 w1 l1) (Layout h2 w2 l2)
    = Layout (h1 + h2) (max w1 (l1 + w2)) (l1 + l2)
