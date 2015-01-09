{-# LANGUAGE ExistentialQuantification #-}
module Csp where

    import Data.List
    import Env
    import qualified Data.Set as Set
    import Common
    
{-  predOfEvent (Out _ p) = p
    prefOfEvent (In _ _) = False


   
    toTable :: Proc -> ([Event],[Event]) -- (IN, OUT)
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

    updateTree :: String -> Proc -> (Proc, [Func])
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
                                                                
    step :: Proc -> Proc
    step t = let e = (admEvents . toTable) t
                 (newT, fs) = updateTree e t
             in newT
             -}

    chkNames :: [ProcDef] -> IO Bool
    chkNames [] =  return True
    chkNames ((Def name p):ds) = case find (\(Def name' p') -> name == name') ds of
                                            Just (Def n _) -> do putStrLn ("Error: multiples definiciones de "++n++".")
                                                                 return False
                                            Nothing        -> chkNames ds
    
    
    
    sistema :: Env -> IO Proc
    sistema e = return $ maybe (maybe Stop id (envLookup "Sistema" e)) id (envLookup "SISTEMA" e)                     
                    
    alpha :: Proc -> Env -> Set.Set Event
    alpha p e = snd $ alpha' ["Sistema","SISTEMA"] p e
    
    alpha' :: [String] -> Proc -> Env -> ([String], Set.Set Event)
    alpha' v Skip _ = (v, Set.empty) -- revisar
    alpha' v Stop _ = (v, Set.empty)
    alpha' v (Ref s) e = if elem s v then (v, Set.empty)
                                     else case envLookup s e of
                                            Just p -> alpha' (s:v) p e
                                            Nothing -> error "Referencia a proceso inexistente"
    alpha' v (Prefix ev p) e = let (v', a) = alpha' v p e in (v', Set.insert ev a)
    alpha' v (Parallel l r) e = let (v', al) = alpha' v l e
                                    (v'', ar) = alpha' v' r e
                                in (v'', Set.union al ar)
    alpha' v (ExtSel ps) e = foldl (\(v',a) p -> let (v'', a') = alpha' v' p e in (v'', Set.union a a')) (v, Set.empty) ps

    
    menu :: Proc -> Env -> Set.Set Event
    menu p e = snd $ menu' ["Sistema","SISTEMA"] p e
    
    
    menu' :: [String] -> Proc -> Env -> ([String], Set.Set Event)
    menu' v Skip _           = (v, Set.empty) -- revisar
    menu' v Stop _           = (v, Set.empty)
    menu' v (Prefix e p) _   = (v, Set.singleton e)
    menu' v (Ref s) e        = if elem s v then (v, Set.empty)
                                           else case envLookup s e of
                                                Just p -> menu' (s:v) p e
                                                Nothing -> error "Referencia a proceso inexistente"
    menu' v (Parallel l r) e = let (v', ml)  = menu' v l e
                                   (v'', mr) = menu' v' r e
                                   inter = Set.intersection ml mr
                                   a = Set.difference ml (alpha r e)
                                   b = Set.difference mr (alpha l e)
                               in (v'', Set.union inter (Set.union a b))
    menu' v (ExtSel ps) e    = foldl (\(v',a) p -> let (v'', a') = menu' v' p e in (v'', Set.union a a')) (v, Set.empty) ps
    

--    eval' :: [String] -> Proc -> Event -> ([String], Proc)
    
    eval :: Proc -> Event -> Env -> Proc
    eval Skip _ _          = Skip
    eval Stop _ _          = Stop
    eval (Prefix e p) e' _ = if e == e' then p else Stop
    eval (Ref s) e en      = case envLookup s en of
                                  Just p -> eval p e en                                       -- revisar problema de recursion
                                  Nothing -> error "Referencia a proceso inexistente" 
    eval (Parallel l r) e en = case (Set.member e (menu l en), Set.member e (menu r en)) of
                                 (True, True)   -> Parallel (eval l e en) (eval r e en)                               -- sincronizacion
                                 (True, False)  -> if Set.member e (alpha r en) then Stop                             -- deadlock
                                                                                else Parallel (eval l e en) r         -- IEOF
                                 (False, True)  -> if Set.member e (alpha l en) then Stop                             -- deadlock
                                                                                else Parallel l (eval r e en)         -- IEOF
                                 (False, False) -> Stop                                                               -- error
    eval (ExtSel ps) e en   = maybe Stop (\p -> eval p e en) (find (\p -> Set.member e (menu p en)) ps)
    
    
  {-  
    setRefs :: [ProcDef] -> IO [ProcDef]
    setRefs ds = return $ map (\(Def name p) -> Def name (setRefsProc ds p)) ds
                            where setRefsProc defs (Ref s _) = case find (\(Def n _) -> n == s) defs of
                                                                    Nothing        -> Stop -- error de compilacion
                                                                    Just (Def n p) -> Ref s p                       -- deberia chequear que no haya P = P
                                  setRefsProc defs (Prefix r p) = Prefix r (setRefsProc defs p)
                                  setRefsProc defs (Parallel l r) = Parallel (setRefsProc defs l) (setRefsProc defs r)
                                  setRefsProc defs (ExtSel ps) = ExtSel $ map (\p -> setRefsProc defs p) ps
                                  setRefsProc _ p = p
    
    

        let p' = setRefsProc p p'
            

        setRefs [] bs     = bs
      setRefs ((Def name p):ds) bs = let setRefsProc (Ref s _) (n, pr) vs = if s == n then Ref s pr
                                                                                      else case find (\(Def na p') -> na == s) vs of
                                                                                            Just (Def n pr) -> Ref s pr
                                                                                            Nothing -> Stop
                                                                               -- deberia chequear que no haya P = P
                                       setRefsProc (Prefix r p) pr vs = Prefix r (setRefsProc defs p)
                                       setRefsProc (Parallel l r) pr vs = Parallel (setRefsProc defs l) (setRefsProc defs r)
                                       setRefsProc (ExtSel ps) pr vs = ExtSel $ map (\p -> setRefsProc defs p) ps
                                       setRefsProc pr _ = pr
                                       p' = setRefsProc p (name, p') bs
                                   in  (Def name p'):(setRefs ds)
                                   
                                   -}                                     
    
----------------------------
--- Print
----------------------------
    
    
    strOfEvent (Out s _) = s
    strOfEvent (In s _) = s

    printEvent (Out s p) = "_out_ "++s++" "
    printEvent (In s f) = "_in_ "++s++" "
             
    strProc Skip = "skip;"
    strProc Stop = "stop;"
    strProc (Ref s) = "ref a "++ s
    strProc (Prefix e p) = strOfEvent e ++ " -> " ++ strProc p
    strProc (Parallel l r) = strProc l ++ " || " ++ strProc r
    strProc (ExtSel ps) = foldl (\s p -> s ++ " [] " ++ strProc p) "" ps

    printProc :: Proc -> IO ()
    printProc p = putStrLn $ strProc p
    
    strDefs :: [ProcDef] -> [String]
    strDefs ds = map strdef ds
        where strdef (Def name p) = name ++ " = " ++ (strProc p)
    
    printDefs :: [ProcDef] -> IO ()
    printDefs ds = mapM_ putStrLn (strDefs ds)
