{-# LANGUAGE NoMonomorphismRestriction #-}

import Text.Megaparsec
import Text.Megaparsec.Lexer

data TN = T Char | N Rule
type Prod = [TN]
type Rule = [Prod]

instance Show TN where
  show (T c) = show c
  show (N _) = "<N>"

nil :: Rule
nil = []

eps :: Prod
eps = []

digits :: Rule
digits = [ [ T '0' ], [ T '1' ] ]

diffs :: String -> Rule -> Rule
diffs = flip (foldr diff)

diff, diffAlt :: Char -> Rule -> Rule
diff = diffAlt

diffAlt c = concatMap (diffProd c)

diffProd :: Char -> Prod -> Rule
diffProd c [] = nil
diffProd c (T c' : _ ) | c /= c' = nil
diffProd c (T c' : pr) | c == c' = [pr]
diffProd c (N n  : pr) =
  let n' = diff c n in
  --if isEps n' then [pr] else
  (if canEps n then n' ++ diffProd c pr else n')
  -- case (n', canEps n') of

canEps :: Rule -> Bool
canEps = any (all canEpsTN)
canEpsTN :: TN -> Bool
canEpsTN (T _) = False
canEpsTN (N n) = canEps n

isEps :: Rule -> Bool
isEps [[]] = True
isEps _ = False

num, num1, parens :: Rule
num = [ eps, [ N digits, N num ] ]
num1 = [[N digits, N num]]
parens = [ eps, [ T '[', N parens, T ']' ] ]

shRule :: Int -> Rule -> String
shRule n [] = "nil"
shRule n [[]] = "eps"
shRule n (prod:prods) = shProd n prod ++ concatMap ((" | " ++) . shProd n) prods

shProd :: Int -> Prod -> String
shProd n [] = "eps"
shProd n [x] = shTN n x
shProd n x = concatMap (shTN n) $ x

shTN :: Int -> TN -> String
shTN _ (T c) = show c
shTN 0 (N _) = "<?>"
shTN n (N r) = "(" ++ shRule (pred n) r ++ ")"

prr = putStrLn . shRule 3
