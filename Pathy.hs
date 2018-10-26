import Data.Monoid

-- Representation of file paths.
-- Absolute paths are a list of components.
-- Absolute paths are of form /(COMPONENT)*
-- Relative paths have a list of components and the number of levels to go up.
-- Relative paths are of form (../)*(COMPONENT)*
-- e.g. Rel 3 ["foo", "bar"] corresopnds to path "../../../foo/bar"
--      Abs ["baz"] corresponds to path "/baz"
data Path
    = Abs { pathComponents :: [String] }
    | Rel { pathUpLevels :: Int, pathComponents :: [String] }
    deriving Eq

-- empty path corresponds to "."
emptyPath = Rel 0 []
-- root path corresponds to "/"
rootPath = Abs []
-- up a level path corresponds to ".."
upLevelPath = Rel 1 []
-- Singleton relative path for 'foo' corresopnds to "foo"
singletonPath = Rel 0 . pure

-- implementation details
dropCompsImpl :: [a] -> Int -> [a] -> (Int, [a])
dropCompsImpl (c:rpth) 0 pth = dropCompsImpl rpth 0 (c:pth)
dropCompsImpl (_:rpth) n pth = dropCompsImpl rpth (pred n) pth
dropCompsImpl [] n pth = (n, pth)
dropComps :: [a] -> Int -> [a] -> (Int, [a])
dropComps pthA 0 pthB = (0, pthA ++ pthB)
dropComps pthA n pthB = dropCompsImpl (reverse pthA) n pthB

-- Monoid for paths is path concatenation.
-- It automatically resolves any up-a-level components in relative paths.
-- e.g. path "/foo/bar" <> path "../baz" == path "/foo/baz"
-- Appending an absolute path to any path is the absolute path on the right.
-- e.g. path "foo/boo" <> path "/bar" == path "/bar"
instance Monoid Path where
    mempty = emptyPath
    mappend _ path@(Abs _) = path
    mappend (Abs pthA) (Rel n pthB) =
      let (_, pth) = dropComps pthA n pthB in Abs pth
    mappend (Rel nA pthA) (Rel nB pthB) =
      let (n, pth) = dropComps pthA nB pthB in Rel (nA + n) pth

-- Simple path parsing
splitSlash :: String -> [String]
splitSlash "" = []
splitSlash str = let (c, cs) = break (=='/') str in c : splitSlash (drop 1 cs)
pathComp :: String -> Path
pathComp "" = emptyPath
pathComp "." = emptyPath
pathComp ".." = upLevelPath
pathComp c = singletonPath c
pathComps :: String -> [Path]
pathComps = map pathComp . splitSlash
relPath :: String -> Path
relPath = mconcat . pathComps
path :: String -> Path
path ('/':p) = rootPath <> relPath p
path p = relPath p

-- Simple path printing
showPath :: Path -> String
showPath (Rel 0 []) = "."
showPath (Rel n cs) = drop 1 (showCompsAbs (replicate n ".." ++ cs))
showPath (Abs []) = "/"
showPath (Abs cs) = showCompsAbs cs
showCompsAbs = concatMap ('/':)

instance Show Path where
  show p = "path " ++ show (showPath p)

-- A regular semigroup
-- https://en.wikipedia.org/wiki/Regular_semigroup
class RegularSemigroup a where
  -- An associative binary operation
  (</>) :: a -> a -> a
  -- A pseudo-inverse and an inverse.
  -- These need not be unique, hence the indefinite article.
  aPseudoInverse :: a -> a
  aPseudoInverse = anInverse
  anInverse :: a -> a
  anInverse x = let px = aPseudoInverse x in px </> x </> px

-- Inverse and pseudo-inverse laws:
check_aPseudoInverse, check_anInverse :: (Eq a, RegularSemigroup a) => a -> Bool
check_aPseudoInverse a = (a </> aPseudoInverse a </> a) == a
check_anInverse a = let b = anInverse a
                    in (a </> b </> a) == a && (b </> a </> b) == b

-- Path is a regular semigroup
instance RegularSemigroup Path where
  (</>) = mappend
  -- Any absolute path is an inverse of an absolute path
  -- There seem to be two sensibly looking options:
  --  1) The original path
  --  2) The root path
  anInverse p@(Abs _) = p
  -- Relative inverse has to undo level-ups and components
  -- The path component names may (must?) be fabricated, we use numbers here.
  anInverse (Rel n cs) = Rel (length cs) [show d | d <- [1..n]]
