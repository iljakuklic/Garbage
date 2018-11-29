{-# LANGUAGE UnboxedTuples, KindSignatures, NoImplicitPrelude, MagicHash, PolyKinds, BangPatterns #-}

module Mnau where

import GHC.Prim
import GHC.ST
import GHC.Types
import Prelude(($), (-), (*), (+), error, foldr, (==), succ)

arrayError = error "Uninitialized array element"

data IntArray a = IntArray (SmallArray# a)

intArray :: Int -> (Int -> a) -> IntArray a
intArray size@(I# size#) init = runSTRep $ \s0 -> let
    (# sNew, aryNew #) = newSmallArray# size# arrayError s0
    {-# INLINE writeElem #-}
    writeElem x@(I# x#) = writeSmallArray# aryNew x# (init x)
    {-# INLINE fill #-}
    fill n | n == size = \s -> s
    fill n = \s -> writeElem n (fill (succ n) s)
    sFilled = fill 0 sNew
    (# sDone, aryDone #) = unsafeFreezeSmallArray# aryNew sFilled
  in (# sDone, IntArray aryDone #)

range :: Int -> [Int]
range n = [0..(n-1)]

arySize (IntArray a#) = I# (sizeofSmallArray# a#)
get (IntArray a#) (I# i#) = let (# r #) = indexSmallArray# a# i# in r
