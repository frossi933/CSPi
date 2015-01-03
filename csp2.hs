{-# LANGUAGE ExistentialQuantification #-}

import Data.List
--data Func = forall a b. (a -> b)
data Func = IO ()
data Event = Out String Bool | In String Func

data Id a = Id a deriving(Show)
newtype State s a = State { runState :: s -> (a,s) }

instance Monad Id where
    return x = Id x
    Id x >>= f = f x

instance Monad (State s) where
    return x = State $ \s -> (x,s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in  g newState

--data Proc = Skip | Stop | PRec | P [Event] Proc
--data TreeProc = Unit Proc | Parallel [TreeProc] | IntSel [TreeProc]
                
data Proc = Skip | Stop | Ref Proc | Prefix Event Proc | Parallel [Proc] | ExtSel [Proc]

                
                
eventOut :: String -> Bool -> State Proc ()
eventOut e b = State $ \p -> case p of
                                    Skip -> ((),P [Out e b] Skip)
                                    Stop -> ((),P [Out e b] Stop)
                                    PRec -> ((),P [Out e b] PRec)
                                    P es p' -> ((),P ((Out e b):es) p')
                                    
eventIn :: String -> (a -> b) -> State Proc ()
eventIn e f = State $ \p -> case p of
                                    Skip -> ((),P [In e f] Skip)
                                    Stop -> ((),P [In e f] Stop)
                                    PRec -> ((),P [In e f] PRec)
                                    P es p' -> ((),P ((In e f):es) p')
                                    
                                    
procSkip = State $ \_ -> ((), Skip)
procStop = State $ \_ -> ((), Stop)

strOfEvent (Out s _) = s
strOfEvent (In s _) = s

predOfEvent (Out _ p) = p
prefOfEvent (In _ _) = False

printProc Skip = "skip;"
printProc Stop = "stop;"
printProc (P es p) = (foldl (\e s -> e++" -> "++(strOfEvent s)) "" es) ++ " -> "++printProc p
printProc PRec = "rec"

printEvent (Out s p) = "_out_ "++s++" "
printEvent (In s f) = "_in_ "++s++" "


dk_boton :: State Proc ()
dk_boton = do eventOut "press" True
              
dk_lamp = do eventIn "on" id
             eventIn "off" id

lamp :: State Proc ()
lamp = do eventIn "press" id
          eventOut "on" True
          eventIn "unpress" id
          eventOut "off" True

boton_proc :: Proc
boton_proc = let P es p = snd $ (runState dk_boton) PRec 
             in P (reverse es) p
          
dk_lamp_proc :: Proc
dk_lamp_proc = let P es p = snd $ (runState dk_lamp) PRec 
               in P (reverse es) p

          
lamp_proc :: Proc
lamp_proc = let P es p = snd $ (runState lamp) PRec 
           in P (reverse es) p
           


toTable :: TreeProc -> ([Event],[Event]) -- (IN, OUT)
toTable (Unit Skip) = ([],[])
toTable (Unit Stop) = ([],[])
toTable (Unit PRec) = ([],[])
toTable (Unit (P es p)) = case head es of
                               Out s p -> ([],[Out s p])
                               In s p -> ([In s p],[])
toTable (Parallel ps) = foldl (\(i,o) p -> let (i',o')=toTable p in (i++i',o++o')) ([],[]) ps 
toTable (IntSel ps) = foldl (\(i,o) p -> let (i',o')=toTable p in (i++i',o++o')) ([],[]) ps


-- (INs, OUTs) -> evento
admEvents :: ([Event],[Event]) -> String--(Event,[Event])
admEvents (is,os) = admEventsAux (is, os) os
    where admEventsAux (is, []) os' = admEvents (is,os') -- ver de poner un wait o algo
          admEventsAux (is, o:os) os' = if predOfEvent o && any (\e -> strOfEvent e == strOfEvent o) is then strOfEvent o 
                                                                                                        else admEventsAux (is, os) os' 

updateTree :: String -> TreeProc -> (TreeProc, [Func])
updateTree s (Unit Skip) = (Unit Skip,[])
updateTree s (Unit Stop) = (Unit Stop,[])
updateTree s (Unit PRec) = (Unit PRec,[])
updateTree s (Unit (P es p)) = if (strOfEvent . head) es == s then case es of
                                                                      [] -> undefined -- no deberia pasar nunca
                                                                      [Out e b] -> if b then (Unit p,[]) else (Unit (P es p),[]) -- al menos q p sea un PRec
                                                                      [In e f] -> (Unit p, [f]) -- deberia ejecutar f y ver q p no sea PRec
                                                                      (Out e b):(e':es') -> if b then (Unit (P (e':es') p),[]) 
                                                                                                 else (Unit (P es p),[])
                                                                      (In e f):(e':es') -> (Unit (P (e':es') p), [f]) -- deberia ejecutar f y ver q p no sea PRec
                                                            else (Unit (P es p),[])
updateTree s (Parallel ps) = let (newT, fs) = (map (updateTree s) ps) in (Parallel newT, concat fs)
updateTree s (IntSel ps) = let (newT, fs) = (map (updateTree s) ps) in (IntSel newT, concat fs)
                                                            
                                                            
                                                            
step :: TreeProc -> TreeProc
step t = let e = (admEvents . toTable) t
             (newT, fs) = updateTree e t
         in 