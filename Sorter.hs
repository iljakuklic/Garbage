{-# LANGUAGE NoMonomorphismRestriction, GADTs, RankNTypes,
             StandaloneDeriving, GeneralizedNewtypeDeriving,
             TypeOperators,
             FlexibleInstances, FlexibleContexts,
             MultiParamTypeClasses, ConstraintKinds #-}

import Data.Array.ST
import Data.Array.Base
import Data.Foldable
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

swapMinMax i j = do
    r <- cmpAt i j
    case r of
        GT -> swapAt i j >> return True
        _  -> return False

bubbleSortSimple beg end = do
    for_ [end,end-1..2] $ \stop ->
        for_ [beg..(stop - 1)] $ \i ->
            swapMinMax i (i + 1)

bubbleSort beg end = bubbleSortImpl beg (end - 1)
bubbleSortImpl beg end = do
    Last ii <- flip foldMap [beg..end] $ \i -> do
        sw <- swapMinMax i (i + 1)
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

noSort _ _ = return ()


--------------------------------------------------------------------

drawBars :: Int -> Int -> [(Float, Int)] -> G.Picture
drawBars numBars maxVal entries = pic
  where
    rect :: Int -> Float -> G.Picture
    rect ht x = G.rectangleUpperSolid 0.6 (fromIntegral ht)
    bar x ht = G.translate (x + 0.5) 0 (rect ht x)
    maxHt = fromIntegral maxVal :: Float
    width = fromIntegral numBars :: Float
    pic'' = foldMap (uncurry bar) entries
    pic' = G.scale (2 * recip width) (2 * recip maxHt) pic''
    pic = G.translate (-1) (-1) pic'

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

drawStaticBars :: (Idx -> Bool) -> [(Idx, Int)] -> G.Picture
drawStaticBars pred entries = drawBars numBars maxVal barEntries
  where
    numBars = length entries
    maxVal = maximum (fmap snd entries)
    toBar (idx, val) = (fromIntegral idx :: Float, val)
    barEntries = (fmap toBar (filter (pred . fst) entries))

only idxs idx = idx `elem` idxs

drawStaticState :: [Int] -> AnimState -> (Idx -> Bool) -> G.Picture
drawStaticState vals ste sel = drawStaticBars sel (zip (asOrder ste) vals)

drawHighlight :: [Int] -> AnimState -> (Idx -> Bool) -> G.Color -> G.Picture
drawHighlight vals ste p clr
  = G.color G.orange (drawStaticState vals ste (not . p))
  <> G.color clr (drawStaticState vals ste p)

drawState :: [Int] -> AnimState -> G.Picture
drawState vals ste@(AnimState to ord (AnAction (SwapAt i j) : _)) = pic
  where
    pic = orig <> swapping
    orig = G.color G.orange (drawStaticState vals ste (not . only [i, j]))
    swapping = G.color G.red (drawBars numBars maxVal swEntries)
    numBars = length vals
    maxVal = maximum vals
    valOf idx = fromJust (lookup idx (zip ord vals))
    (xi0, xj0) = (fromIntegral i, fromIntegral j) :: (Float, Float)
    lerp t = (1.0 - t) * xi0 + t * xj0
    swEntries = [(lerp (1.0 - to), valOf i), (lerp to, valOf j)]

drawState vals ste@(AnimState _ _ (AnAction (CmpAt i j) : _))
  = drawHighlight vals ste (only [i,j]) G.red
drawState vals ste = G.color G.orange (drawStaticState vals ste (const True))

drawView :: (Float, Float) -> [Int] -> AnimState -> G.Picture
drawView (sx, sy) vals ste
    = G.scale (0.45 * sx) (0.40 * sy) (drawState vals ste)

actionDuration :: AnAction -> Float
actionDuration (AnAction (PeekAt _)) = 0.01
actionDuration (AnAction (SwapAt i j)) = 0.1 * (max 3 (min dist 10))
  where dist = abs (fromIntegral j - fromIntegral i) :: Float
actionDuration (AnAction (CmpAt _ _)) = 0.2

updateState :: Float -> AnimState -> AnimState
updateState _dt ste@(AnimState _ _ []) = ste
updateState dt ste@(AnimState p ord (a:acts)) = next
  where
    dur = actionDuration a
    dp = dt / actionDuration a
    next =
      if dur <= 0.0
        then updateState dt (AnimState p ord acts)
        else if dp < p
             then ste { asCountdown = asCountdown ste - dp }
             else AnimState 1.0 (updateOrder a ord) acts

main :: IO ()
main = do
    ary <- randomRIO (50, 80) >>= flip replicateM (randomRIO (10, 250))
    let (_, _, acts) = runSort selectSort (mkArray ary)
    let size@(sizex, sizey) = (900, 600)
    let win = G.InWindow "Sorter" size (50, 50)
    let initSte = AnimState {
        asCountdown = 1.0,
        asOrder = take (length ary) [0..],
        asActions = acts
      }
    let run = G.simulate win (G.greyN 0.15) 50
    run initSte (drawView size ary) (const updateState)
