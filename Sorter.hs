{-# LANGUAGE NoMonomorphismRestriction, GADTs, RankNTypes,
             StandaloneDeriving, GeneralizedNewtypeDeriving,
             TypeOperators,
             FlexibleInstances, FlexibleContexts,
             MultiParamTypeClasses, ConstraintKinds #-}

import Data.Array.ST
import Data.Array.Base
import Data.Foldable
import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans
import Control.Monad.Trans.RWS

import qualified Graphics.Gloss as G

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

noSort _ _ = return ()


--------------------------------------------------------------------

vizArray :: Int -> Int -> [(Int, Float)] -> G.Picture
vizArray numBars maxVal entries = pic
  where
    rect :: Int -> Float -> G.Picture
    rect ht x = G.rectangleUpperSolid 0.6 (fromIntegral ht)
    bar ht x = G.translate (x + 0.5) 0 (rect ht x)
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

drawBars :: G.Color -> Int -> Int
         -> (Float, Float) -> [(Int, Float)] -> G.Picture
drawBars clr numBars maxVal (sx, sy) entries
    = G.color clr (G.scale (0.45 * sx) (0.40 * sy)
              (vizArray numBars maxVal entries))

drawState :: (Float, Float) -> [Int] -> AnimState -> G.Picture
drawState dim vals (AnimState _cd ord (AnAction (SwapAt i j) : _)) = statBars
  where
    statEntries = [ (val, fromIntegral idx)
                  | (val, idx) <- zip vals ord, idx /= i, idx /= j]
    statBars = drawBars G.orange (length vals) (maximum vals) dim statEntries
drawState dim vals ste = drawBars G.orange (length vals) (maximum vals) dim ents
  where ents = zip vals (fmap fromIntegral (asOrder ste))

actionDuration :: AnAction -> Float
actionDuration (AnAction (PeekAt _)) = 0.01
actionDuration (AnAction (SwapAt _ _)) = 0.5
actionDuration (AnAction (CmpAt _ _)) = 0.3

updateState :: Float -> AnimState -> AnimState
updateState _dt ste@(AnimState _ _ []) = ste
updateState dt ste@(AnimState p ord (a:acts)) = next
  where
    dp = dt / actionDuration a
    next = if dp < p
             then ste { asCountdown = asCountdown ste - dp }
             else AnimState 1.0 (updateOrder a ord) acts

main :: IO ()
main = do
    let ary = [34,45,27,11,55,30,39,40,41,22]
    let (_, _, acts) = runSort bubbleSort (mkArray ary)
    let size@(sizex, sizey) = (400, 300)
    let win = G.InWindow "Sorter" size (100, 100)
    let initSte = AnimState {
        asCountdown = 1.0,
        asOrder = take (length ary) [0..],
        asActions = acts
      }
    let run = G.simulate win (G.greyN 0.15) 50
    run initSte (drawState size ary) (const updateState)
