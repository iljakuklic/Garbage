{-# LANGUAGE NoMonomorphismRestriction, GADTs, RankNTypes,
             StandaloneDeriving, GeneralizedNewtypeDeriving,
             TypeOperators,
             FlexibleInstances, FlexibleContexts,
             MultiParamTypeClasses, ConstraintKinds #-}

import Data.Array.ST
import Data.Array.Base
import Data.Foldable
import qualified Data.List as L
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans
import Control.Monad.Trans.RWS
import qualified Graphics.Gloss as G
import System.Random

type f ~> g = forall a . f a -> g a

newtype Idx = Idx { getIdx :: Int } deriving (Eq, Ord, Enum, Show)
deriving instance Num Idx
deriving instance Real Idx
deriving instance Integral Idx
deriving instance Ix Idx

data Action a where
    PeekAt :: Idx -> Action Int
    SwapAt :: Idx -> Idx -> Action ()
    CmpAt :: Idx -> Idx -> Action Ordering

deriving instance Show (Action a)

data AnAction = forall a . AnAction { getTheAction :: Action a }
deriving instance Show AnAction

data Sorter a where
    SPure :: a -> Sorter a
    SBind :: Action a -> (a -> Sorter b) -> Sorter b

instance Functor Sorter where
    fmap f (SPure x) = SPure (f x)
    fmap f (SBind x g) = SBind x (fmap f . g)

instance Applicative Sorter where
    pure = SPure
    SPure f <*> xa = fmap f xa
    SBind c fr <*> xa = SBind c (\r -> fr r <*> xa)

instance Monad Sorter where
    return = pure
    SPure x >>= f = f x
    SBind c fr >>= f = SBind c (\r -> fr r >>= f)

instance Semigroup a => Semigroup (Sorter a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Sorter a) where
    mempty = return mempty

act :: Action a -> Sorter a
act a = SBind a SPure

peekAt i = act (PeekAt i)
cmpAt i j = act (CmpAt i j)
swapAt i j = act (SwapAt i j)

runSorter :: Monad m => (Action ~> m) -> Sorter a -> m a
runSorter h (SPure x) = return x
runSorter h (SBind a f) = h a >>= runSorter h . f

type ArrayTy s = STUArray s Idx Int
--type MonadSort s m = MArray (STUArray s) Int m
type SorterMonad s = RWST (ArrayTy s) [AnAction] () (ST s)

handleAction :: Action a -> ArrayTy s -> ST s a
handleAction (PeekAt i) arr = readArray arr i
handleAction (CmpAt i j) arr =
    liftA2 compare (readArray arr i) (readArray arr j)
handleAction (SwapAt i j) arr = do
    xi <- readArray arr i
    xj <- readArray arr j
    writeArray arr i xj
    writeArray arr j xi

handleAndLogAction :: Action a -> SorterMonad s a
handleAndLogAction a = do
    tell [AnAction a]
    ask >>= lift . handleAction a

runSort :: (IArray iarr Int)
        => (Idx -> Idx -> Sorter a)       -- Sorting algo
        -> iarr Idx Int                   -- The array
        -> (a, iarr Idx Int, [AnAction])
runSort sorter inAry = runST $ do
    ary <- thaw inAry
    (lo, hi) <- getBounds ary
    let sorting = runSorter handleAndLogAction (sorter lo hi)
    (res, log) <- evalRWST sorting ary ()
    outAry <- freeze ary
    return (res, outAry, log)

mkArray :: IArray UArray e => [e] -> UArray Idx e
mkArray elts = listArray (Idx 0, Idx (length elts - 1)) elts

sort2 i j = do
    r <- cmpAt i j
    case r of
        GT -> swapAt i j >> return True
        _  -> return False

sort3 i j k = do
    sort2 i j
    sort2 i k
    sort2 j k
    return ()

bubbleSortSimple beg end = do
    for_ [end,end-1..2] $ \stop ->
        for_ [beg..(stop - 1)] $ \i ->
            sort2 i (i + 1)

bubbleSort beg end = bubbleSortImpl beg (end - 1)
bubbleSortImpl beg end = do
    Last ii <- flip foldMap [beg..end] $ \i -> do
        sw <- sort2 i (i + 1)
        return (Last (if sw then Just i else Nothing))
    case ii of
        Just sw -> bubbleSortImpl beg (sw - 1)
        Nothing -> return ()

findMinIdx cur beg end | beg > end = return cur
findMinIdx cur beg end = do
    r <- cmpAt cur beg
    findMinIdx (if r == GT then beg else cur) (succ beg) end

selectSort beg end = do
    for_ [beg..end] $ \i -> do
        minIdx <- findMinIdx i (succ i) end
        when (minIdx /= i) (swapAt i minIdx)

partition i j piv | j >= piv = swapAt i piv >> return i
partition i j piv = do
    r <- cmpAt j piv
    case r of
        LT -> swapAt i j >> partition (i + 1) (j + 1) piv
        _ -> partition i (j + 1) piv

quickSort beg end | beg >= end = return ()
quickSort beg end | beg + 1 == end = sort2 beg (beg + 1) >> return ()
quickSort beg end | beg + 2 == end = sort3 beg (beg + 1) (beg + 2)
quickSort beg end = do
    mid <- partition beg beg end
    quickSort beg (mid - 1)
    quickSort (mid + 1) end

noSort _ _ = return ()


--------------------------------------------------------------------

-- Visual representation of a bar.
data Bar = Bar {
    barPos :: Float,     -- Bar position, standard spacing between bars = 1.0
    barHeight :: Int,    -- Bar height
    barColor :: G.Color  -- Bar color
  } deriving (Show)

drawBar :: Bar -> G.Picture
drawBar (Bar x ht c) = G.translate (x + 0.5) 0 rect
  where
    rect = G.color c (G.rectangleUpperSolid 0.7 (fromIntegral ht))

drawBars :: [Bar] -> G.Picture
drawBars bars = G.translate (-1) (-1) picScaled
  where
    maxHt = fromIntegral (maximum (map barHeight bars)) :: Float
    width = fromIntegral (length bars) :: Float
    pic = foldMap drawBar bars
    picScaled = G.scale (2 * recip width) (2 * recip maxHt) pic

updateOrder :: AnAction -> [Idx] -> [Idx]
updateOrder (AnAction (SwapAt i j)) = fmap updateIdx
  where
    updateIdx n | n == i = j
    updateIdx n | n == j = i
    updateIdx n = n
updateOrder _act = id

data AnimState = AnimState {
    asCountdown :: Float,      -- Progress of the current animation 0.0..1.0
    asOrder :: [Idx],
    asActions :: [AnAction]
  }

{-
drawStaticBars :: (Idx -> Bool) -> [(Idx, Int)] -> G.Picture
drawStaticBars pred entries = drawBars numBars maxVal barEntries
  where
    numBars = length entries
    maxVal = maximum (fmap snd entries)
    toBar (idx, val) = (fromIntegral idx :: Float, val)
    barEntries = (fmap toBar (filter (pred . fst) entries))
-}

only idxs idx = idx `elem` idxs

highlight idxs idx = if idx `elem` idxs then G.red else G.orange

drawBarsWithHighlight :: [Idx] -> [Int] -> [Idx] -> G.Picture
drawBarsWithHighlight ord vals hlx
  = drawBars [ Bar (fromIntegral x) h (highlight hlx x) | (x, h) <- zip ord vals ]

drawState :: [Int] -> AnimState -> G.Picture
drawState vals ste@(AnimState to ord (AnAction (SwapAt i j) : _)) = bars
  where
    bars = drawBars [ Bar (pos ix) y (hl ix) | (ix, y) <- static ++ swapping ]
    (swapping, static) = L.partition (flip elem [i, j] . fst) (zip ord vals)
    lerp t = (1.0 - t) * (fromIntegral i) + t * (fromIntegral j)
    hl = highlight [i, j]
    pos :: Idx -> Float
    pos ix | ix == i = lerp (1.0 - to)
    pos ix | ix == j = lerp to
    pos ix = fromIntegral ix
drawState vals (AnimState _ ord (AnAction (CmpAt i j) : _))
  = drawBarsWithHighlight ord vals [i, j]
drawState vals (AnimState _ ord _)
  = drawBarsWithHighlight ord vals []

drawView :: (Float, Float) -> [Int] -> AnimState -> G.Picture
drawView (sx, sy) vals ste
    = G.scale (0.45 * sx) (0.40 * sy) (drawState vals ste)

actionDuration :: AnAction -> Float
actionDuration (AnAction (PeekAt _)) = 0.01
actionDuration (AnAction (SwapAt i j)) = 0.1 * (max 3 (min dist 10))
  where dist = abs (fromIntegral j - fromIntegral i) :: Float
actionDuration (AnAction (CmpAt _ _)) = 0.1

progressIn a dt = dt / actionDuration a

updateState :: Float -> AnimState -> AnimState
updateState _dt ste@(AnimState _ _ []) = ste
updateState dt ste@(AnimState _ _ (a:acts)) | actionDuration a <= 0.0
  = updateState dt (ste { asActions = acts })
updateState dt ste@(AnimState p ord (a:acts)) | progressIn a dt < p
  = ste { asCountdown = asCountdown ste - progressIn a dt }
updateState dt ste@(AnimState p ord (a:acts))
  = AnimState 1.0 (updateOrder a ord) acts

main :: IO ()
main = do
    ary <- randomRIO (50, 80) >>= flip replicateM (randomRIO (10, 250))
    let (_, _, acts) = runSort quickSort (mkArray ary)
    let size@(sizex, sizey) = (900, 600)
    let win = G.InWindow "Sorter" size (50, 50)
    let initSte = AnimState {
        asCountdown = 1.0,
        asOrder = take (length ary) [0..],
        asActions = acts
      }
    let run = G.simulate win (G.greyN 0.15) 50
    run initSte (drawView size ary) (const updateState)
