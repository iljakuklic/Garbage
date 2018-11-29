{-# LANGUAGE RankNTypes, ConstraintKinds, GADTs #-}

data Box c = forall a . (c a) => Box a

things :: [Box Show]
things = [ Box 5, Box "foo", Box 7.55, Box (7, True) ]

printAll :: [Box Show] -> IO ()
printAll [] = return ()
printAll (Box x : xs) = putStrLn (show x) >> printAll xs

data Obj vtab = forall a . Obj (vtab a) a

newtype Drawable a = Drawable (a -> String)

type DrawObj = Obj Drawable

stars :: Int -> DrawObj
stars = Obj (Drawable (\n -> replicate n '*'))

drawObjs :: [DrawObj]
drawObjs = [ stars 3 ]
