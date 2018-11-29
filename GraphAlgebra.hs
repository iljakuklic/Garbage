{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified Data.Set as S
import Data.Monoid

data Graph a = Graph { gnodes :: S.Set a, gedges :: S.Set (a, a) } deriving (Eq, Show)

empty = Graph S.empty S.empty
node x = Graph (S.singleton x) S.empty
union (Graph n1 e1) (Graph n2 e2) = Graph (n1 `S.union` n2) (e1 `S.union` e2)
connect (Graph n1 e1) (Graph n2 e2) = Graph (n1 `S.union` n2) (e1 `S.union` e2 `S.union` e3)
  where e3 = S.fromList [ (a, b) | a <- S.toList n1, b <- S.toList n2 ]
(~~>) = connect

unions = foldr union empty
g1 ~~~ g2 = union (g1 ~~> g2) (g2 ~~> g1)
edge a b = node a ~~> node b
clique g = g ~~> g
nodes = unions . map node
path ns = unions [ edge a b | (a, b) <- zip ns (drop 1 ns) ]
cycleg [] = empty
cycleg ns@(n:_) = path ns `union` edge (last ns) n

