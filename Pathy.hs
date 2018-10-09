import Data.Monoid

data Path
    = Abs { pathComponents :: [String] }
    | Rel { pathUpLevels :: Int, pathComponents :: [String] }
    deriving Eq

emptyPath = Rel 0 []
rootPath = Abs []
upLevelPath = Rel 1 []
singletonPath = Rel 0 . pure

dropCompsImpl (c:rpth) 0 pth = dropCompsImpl rpth 0 (c:pth)
dropCompsImpl (_:rpth) n pth = dropCompsImpl rpth (pred n) pth
dropCompsImpl [] n pth = (n, pth)
dropComps pthA 0 pthB = (0, pthA ++ pthB)
dropComps pthA n pthB = dropCompsImpl (reverse pthA) n pthB

instance Monoid Path where
    mempty = emptyPath
    mappend _ path@(Abs _) = path
    mappend (Abs pthA) (Rel n pthB) =
      let (_, pth) = dropComps pthA n pthB in Abs pth
    mappend (Rel nA pthA) (Rel nB pthB) =
      let (n, pth) = dropComps pthA nB pthB in Rel (nA + n) pth
  
splitSlash "" = []
splitSlash str = let (c, cs) = break (=='/') str in c : splitSlash (drop 1 cs)
pathComp "" = emptyPath
pathComp "." = emptyPath
pathComp ".." = upLevelPath
pathComp c = singletonPath c
pathComps = map pathComp . splitSlash
relPath = mconcat . pathComps
path ('/':p) = rootPath <> relPath p
path p = relPath p

showCompsAbs = concatMap ('/':)
showPath (Rel 0 []) = "."
showPath (Rel n cs) = drop 1 (showCompsAbs (replicate n ".." ++ cs))
showPath (Abs []) = "/"
showPath (Abs cs) = showCompsAbs cs

instance Show Path where
  show p = "path " ++ show (showPath p)
