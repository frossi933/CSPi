{-# LANGUAGE ExistentialQuantification #-}
module Csp where

    import Data.List
    import qualified Data.Set as Set
    import Common
    import Env
    
    --data Func = forall a b. (a -> b)
{-  predOfEvent (Out _ p) = p
    prefOfEvent (In _ _) = False

    printEvent (Out s p) = "_out_ "++s++" "
    printEvent (In s f) = "_in_ "++s++" "
   
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
    chkNames ((ProcDef name p):ds) = case find (\(ProdDef name' p') -> name == name') ds of
                                            Just (ProcDef n _)-> do putStrLn "Error: multiples definiciones de "++n++"."
                                                                    return False
                                            Nothing -> chkNames ds
    
    sistema :: Env -> Maybe Proc
    sistema m = case envLookup "Sistema" m of
                     Just d -> return d
                     Nothing -> envLookup "SISTEMA" m
                     
                    
    alpha :: Proc -> Env -> Set Event
    alpha p e = snd $ alpha' ["Sistema","SISTEMA"] p e
    
    alpha' :: [String] -> Proc -> Env -> ([String], Set Event)
    alpha' v Skip e = (v, Set.empty) -- revisar
    alpha' v Stop e = (v, Set.empty)
    alpha' v (Ref s) e = if elem s v then (v, Set.empty) else alpha' (s:v) (envLookup s e) e  
    alpha' v (Prefix ev p) e = Set.insert ev (alpha' v p e)
    alpha' v (Parallel ps) e = foldl (\a p -> Set.union (alpha' v p e) a) [] ps
    alpha' v (ExtSel ps) e = foldl (\a p -> Set.union (alpha' v p e) a) [] ps
    
    nextStep :: Proc -> Env -> Scheduler -> Maybe Proc
    nextStep Skip _ _ = Just Skip -- poner un fin exitoso
    nextStep Stop _ _ = Nothing
    nextStep (Ref s) e _ = envLookup s e
    nextStep (Prefix (Out e pr) p) _ _ = do loop (do while pr
                                                     return ())
                                            return p
    nextStep (Prefix (In e f) p) _ _ = do f
                                          return p
    nextStep (Parallel ps) _ _ = case take 2 ps of
                                      [Stop
                                      [Prefix e p, Prefix e' p'] -> 
    
    
    
----------------------------
--- Print
----------------------------
    
    
    strOfEvent (Out s _) = s
    strOfEvent (In s _) = s

             
    strProc Skip = "skip;"
    strProc Stop = "stop;"
    strProc (Ref p) = "ref a: "++ p
    strProc (Prefix e p) = strOfEvent e ++ " -> " ++ strProc p
    strProc (Parallel ps) = foldl (\s p -> s ++ " || " ++ strProc p) "" ps
    strProc (ExtSel ps) = foldl (\s p -> s ++ " [] " ++ strProc p) "" ps

    printProc :: Proc -> IO ()
    printProc p = putStrLn $ strProc p
    
    strDefs :: [ProcDef] -> [String]
    strDefs ds = map strdef ds
        where strdef (ProcDef name p) = name ++ " = " ++ (strProc p)
    
    printDefs :: [ProcDef] -> IO ()
    printDefs ds = mapM_ putStrLn (strDefs ds)
