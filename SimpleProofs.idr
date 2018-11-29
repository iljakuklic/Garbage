
transport : (P : a -> Type) -> x = y -> P x -> P y
transport _ Refl = id

eqap : f = g -> x = y -> f x = g y
eqap Refl Refl = Refl

eqrefl : x = x
eqrefl = Refl

eqsym : x = y -> y = x
eqsym Refl = Refl

eqtrans : x = y -> y = z -> x = z
eqtrans Refl Refl = Refl

ohso : Bool -> Type
ohso False = Void
ohso True = ()

SnotZ : Z = S x -> Void
SnotZ p = transport (ohso . (== Z)) p ()

dcurry : {A : Type} -> {B : A -> Type} -> {F : (x : A) -> (y : B x) -> Type}
      -> ((xy : DPair A B) -> F (fst xy) (snd xy)) -> (x : A) -> (y : B x) -> F x y
dcurry f x y = f (MkDPair x y)

duncurry : {A : Type} -> {B : A -> Type} -> {F : (x : A) -> (y : B x) -> Type}
        -> ((x : A) -> (y : B x) -> F x y) -> ((xy : DPair A B) -> F (fst xy) (snd xy))
duncurry f (MkDPair x y) = f x y

