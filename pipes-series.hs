
{-# LANGUAGE NoMonomorphismRestriction #-}
-- Series implemented in terms of pipes

module Series where

import Prelude hiding(iterate, id, (.))
import Control.Category
import Pipes
import qualified Pipes.Prelude as P

iter fNext x = yield x >> iter fNext (fNext x)

naturals = iter succ 0

generate x0 fNext fOut = iter fNext x0 >-> P.map fOut

cumsum = P.scan (+) 0 id

series x0 fNext fOut = generate x0 fNext fOut >-> cumsum

pairwise0 x0 = await >>= \x1 -> yield (x0, x1) >> pairwise0 x1

pairwise = await >>= pairwise0

epsilon eps = pairwise >-> P.takeWhile (uncurry (-) >>> abs >>> (>eps)) >-> P.map snd

divergentLength base guess x y | digit' base guess x == digit' base guess y = guess
                               | otherwise = divergentLength base (guess + 1) x y

waitForDigit base idx = P.takeWhile

digit' base idx x | x < 0 = digit base (-idx) (recip x)
digit' base idx x = (x / fromIntegral (base ^ idx))
digit  base idx x = digit' base idx x `mod` fromIntegral base
