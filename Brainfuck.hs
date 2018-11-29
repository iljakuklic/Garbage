
import Data.Word
import Data.Int
import qualified Data.IntMap as M

type Cell = Int8

-- Brainfuck IR
data BFInst = Add Cell | Move Int | Loop BFProg deriving (Show)
type BFProg = [BFInst]

bfParse "" = ("", [])
bfParse ('+':p) = fmap (Add 1 :) (bfParse p)
bfParse ('-':p) = fmap (Add (-1) :) (bfParse p)
bfParse ('>':p) = fmap (Move 1 :) (bfParse p)
bfParse ('<':p) = fmap (Move (-1) :) (bfParse p)
bfParse ('[':p) = let (r, b) = bfParse p in fmap (Loop b :) (bfParse r)
bfParse (']':p) = (p, [])
bfParse (_:p) = bfParse p

bf = snd . bfParse

bfShow [] = ""
bfShow (Add n : is) | n > 0 = replicate (fromIntegral n) '+' ++ bfShow is
bfShow (Add n : is) = replicate (-fromIntegral n) '-' ++ bfShow is
bfShow (Move n : is) | n > 0 = replicate n '>' ++ bfShow is
bfShow (Move n : is) = replicate (negate n) '<' ++ bfShow is
bfShow (Loop loop : is) = '[' : bfShow loop ++ ']' : bfShow is

type Tape = M.IntMap Cell
type BFState = (Int, Tape)

bfEval :: BFProg -> BFState -> BFState
bfEval [] s = s
bfEval (Add n : is) (i, t) = (i, M.alter (maybe (Just n) (Just . (n+))) i t)
bfEval (Move n : is) (i, t) = (i + n, t)
bfEval p@(Loop l : is) (i, t) =
  if M.findWithDefault 0 i t == 0
    then bfEval is (i, t)
    else bfEval (l ++ p) (i, t)
