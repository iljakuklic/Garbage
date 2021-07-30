-- Various bitcoin script representations.
 {-# LANGUAGE TypeOperators #-}
 {-# LANGUAGE GADTs #-}
 {-# LANGUAGE RankNTypes #-}
 {-# LANGUAGE StandaloneDeriving #-}

import Prelude hiding((.), id)
import Control.Category

type a :- b = (a, b)
infixr 4 :-

class BtcEq a where
class Show a => BtcType a where
instance BtcType Int
instance BtcType Bool

data BtcOp a b where
    -- Control flow
    OpNop :: BtcOp r r
    OpIf :: BtcScript r q -> BtcScript r q -> BtcOp (Bool :- r) q
    OpNotIf :: BtcScript r q -> BtcScript r q -> BtcOp (Bool :- r) q
    OpVerify :: BtcOp (Bool :- r) r
    OpReturn :: BtcOp r r'

    -- Constants
    OpPush :: BtcType a => a -> BtcOp r (a :- r)

    -- Stack manipulation
    OpDrop :: BtcOp (a :- r) r
    OpDup :: BtcOp (a :- r) (a :- a :- r)
    OpNip :: BtcOp (a :- b :- r) (a :- r)
    OpOver :: BtcOp (a :- b :- r) (b :- a :- b :- r)
    OpRot :: BtcOp (a :- b :- c :- r) (c :- a :- b :- r)
    OpTuck :: BtcOp (a :- b :- r) (a :- b :- a :- r)
    OpSwap :: BtcOp (a :- b :- r) (b :- a :- r)

    -- Arithmetic
    OpEqual :: BtcEq a => BtcOp (a :- a :- r) (Bool :- r)
    OpNumEqual :: BtcOp (Int :- Int :- r) (Bool :- r)
    OpAdd :: BtcOp (Int :- Int :- r) (Int :- r)
    OpSub :: BtcOp (Int :- Int :- r) (Int :- r)
    OpAbs :: BtcOp (Int :- r) (Int :- r)
    OpNegate :: BtcOp (Int :- r) (Int :- r)

showOp :: BtcOp a b -> String
showOp OpAdd = "OP_ADD"
showOp OpDup = "OP_DUP"
showOp OpNip = "OP_NIP"
showOp OpDrop = "OP_DROP"
showOp OpOver = "OP_OVER"
showOp (OpPush x) = "OP_PUSH(" ++ show x ++ ")"
showOp _ = "???"

newtype BtcScript a b = BtcScript {
    runBtcScript :: forall r . Category r => (forall x y . BtcOp x y -> r x y) -> r a b }

instance Category BtcScript where
    id = BtcScript (const id)
    BtcScript fs . BtcScript gs = BtcScript (\a -> fs a . gs a)

op :: BtcOp a b -> BtcScript a b
op o = BtcScript (\f -> f o)

(???), (??!) :: BtcScript r q -> BtcScript r q -> BtcScript (Bool :- r) q
thn ??? els = op (OpIf thn els)
thn ??! els = op (OpNotIf thn els)
infixr 2 ???, ??!

ifTrue, ifFalse :: BtcScript r r -> BtcScript (Bool :- r) r
ifTrue thn = thn ??? id
ifFalse thn = thn ??! id

mulBy :: Int -> BtcScript (Int :- r) (Int :- r)
mulBy 0 = op OpDrop >>> op (OpPush 0)
mulBy 1 = id
mulBy n | n < 0 = mulBy (negate n) >>> op OpNegate
mulBy n = op OpDup >>> mulBy' n >>> op OpNip

mulBy' :: Int -> BtcScript (Int :- Int :- r) (Int :- Int :- r)
mulBy' 1 = id
mulBy' n = let (q, r) = quotRem n 2 in
       mulBy' q >>> op OpDup >>> op OpAdd >>> (if r == 1 then op OpOver >>> op OpAdd else id)

newtype CatMonoid m a b = CatMonoid m deriving Show
instance Monoid m => Category (CatMonoid m) where
    id = CatMonoid mempty
    CatMonoid a . CatMonoid b = CatMonoid (b <> a)
