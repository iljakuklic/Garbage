
import Data.Time

data Act a = Ret a
           | GetLine      (String  -> Act a)
           | Write String (()      -> Act a)
           | GetTime      (UTCTime -> Act a)

ret = Ret
getline = GetLine Ret
write s = Write s Ret
gettime = GetTime Ret

mapAct :: (a -> b) -> Act a -> Act b
mapAct f (Ret a) = Ret (f a)
mapAct f (GetLine cont) = GetLine (mapAct f . cont)
mapAct f (GetTime cont) = GetTime (mapAct f . cont)
mapAct f (Write s cont) = Write s (mapAct f . cont)

joinAct :: Act (Act a) -> Act a
joinAct (Ret a) = a
joinAct (GetLine cont) = GetLine (joinAct . cont)
joinAct (GetTime cont) = GetTime (joinAct . cont)
joinAct (Write s cont) = Write s (joinAct . cont)

act1 :: (a -> Act b) -> (Act a -> Act b)
act1 f a = joinAct (mapAct f a)
act2 :: (a -> b -> Act c) -> (Act a -> Act b -> Act c)
act2 f a b = act1 (\a' -> act1 (\b' -> f a' b') b) a
($>) = flip act1

writeln s = write (s ++ "\n")

prog1 :: Act Int
prog1 = Write "Your name? " $ \() ->
        GetLine $ \name ->
        Write ("Hello, " ++ name ++ "!\n") $ \() ->
        Ret (length name)
        
prog2 = prog2N 0
prog2N :: Int -> Act Int
prog2N n = Write "Your name? " (\() ->
           GetLine (\name ->
                if not (null name)
                    then
                        Write ("Hello, " ++ name ++ "!\n") (\() ->
                        prog2N (succ n))
                    else
                        Ret n))

prog3 = prog3N 0
prog3N :: Int -> Act Int
prog3N n = write "Your name?" $> \() -> getline $> \name ->
               if not (null name)
                  then
                    write ("Hello, " ++ name ++ "!\n") $> \() -> prog3N (succ n)
                  else 
                    ret n

runAct :: Act a -> IO a
runAct (Ret x) = return x
runAct (GetLine cont) = getLine >>= runAct . cont
runAct (Write s cont) = putStr s >>= runAct . cont

simAct :: Act a -> [String] -> (String, a)
simAct (Ret a) _ = ("", a)
simAct (GetLine cont) (inp:inps) = simAct (cont inp) inps
simAct (GetLine cont) [] = simAct (cont "") []
simAct (Write str cont) inps = let (out, r) = simAct (cont ()) inps in (str ++ out, r)
