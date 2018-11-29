
import qualified Linear   as L
import qualified Linear.V as L
import           Linear.V as L(V2, V3, V4)

instance Num b => Num (a -> b) where
	(f + g) x = f x + g x
	(f * g) x = f x * g x
	signum g = signum . g
	abs g = abs . g
	fromInteger x = const (fromInteger x)


type Distance = Float
type Time = Float

type V2 = L.V2 Float
type V3 = L.V3 Float
type V4 = L.V4 Float

type Model = V3 -> Distance
type Movie a = Time -> a

sphere :: Model
sphere p = L.norm p - 1

trans :: V3 -> Model -> Model
trans d m p = m (p - d)

scale :: Float -> Model -> Model
scale s m p = m (p L.^/ s) * s

(<+>) :: Model -> Model -> Model
(m <+> n) p = min (m p) (n p)
