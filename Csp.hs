{-# LANGUAGE ExistentialQuantification #-}
module Csp where

    import Data.List
    import Env
    import qualified Data.Set as Set
    import Common
    import Exec
   

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
                                            Nothing -> error $ "Referencia a proceso "++s++" inexistente"
    alpha' v (Prefix ev p) e = let (v', a) = alpha' v p e in (v', Set.insert ev a)
    alpha' v (Parallel l r) e = let (v', al) = alpha' v l e
                                    (v'', ar) = alpha' v' r e
                                in (v'', Set.union al ar)
    alpha' v (ExtSel ps) e = foldl (\(v',a) p -> let (v'', a') = alpha' v' p e in (v'', Set.union a a')) (v, Set.empty) ps
    alpha' v (IntSel l r) e = let (v', al) = alpha' v l e
                                  (v'', ar) = alpha' v' r e
                              in (v'', Set.union al ar)
    alpha' v (Seq l r) e = let (v', al) = alpha' v l e
                               (v'', ar) = alpha' v' r e
                           in (v'', Set.union al ar)
    alpha' v (Inter l r) e = let (v', al) = alpha' v l e
                                 (v'', ar) = alpha' v' r e
                             in (v'', Set.union al ar)

    
    menu :: Proc -> Env -> Set.Set Event
    menu p e = let (_, (ev, c)) = menu' ["Sistema","SISTEMA"] p e
                   com = Set.filter (\ch -> case ch of
                                                    Com _ _ -> True
                                                    _       -> False) c
               in Set.union ev $ Set.map C com
    
    menuCom :: Proc -> Env -> Set.Set Channel
    menuCom p e = snd $ snd $ menu' ["Sistema", "SISTEMA"] p e
    
    -- devolvemos:
    --              (lista de strings con las referencias ya visitadas, (conjunto de eventos in y out del menu, conjuto de canales del menu))
    menu' :: [String] -> Proc -> Env -> ([String], (Set.Set Event, Set.Set Channel))
    menu' v Skip _                      = (v, (Set.empty, Set.empty)) -- revisar
    menu' v Stop _                      = (v, (Set.empty, Set.empty))
    menu' v (Prefix e@(In _ _) p) _     = (v, (Set.singleton e, Set.empty))
    menu' v (Prefix e@(Out _ _) p) _    = (v, (Set.singleton e, Set.empty))
    menu' v (Prefix e@(C c) p) _        = (v, (Set.empty, Set.singleton c))  
    menu' v (Ref s) e                   = if elem s v then (v, (Set.empty, Set.empty))                      -- ya lo visitamos
                                                      else case envLookup s e of
                                                            Just p  -> menu' (s:v) p e
                                                            Nothing -> error $ "Referencia a proceso "++s++" inexistente"
    menu' v (Parallel l r) e            = let (v', (mle, mlc))  = menu' v l e
                                              (v'', (mre, mrc)) = menu' v' r e
                                              inter = Set.intersection mle mre
                                              a = Set.difference mle (alpha r e)
                                              b = Set.difference mre (alpha l e)
                                              chan = chanMerge mlc mrc
                                          in (v'', (Set.unions [inter, a, b], chan))
    menu' v (ExtSel ps) e               = foldl (\(v',(ae, ac)) p -> let (v'', (ae', ac')) = menu' v' p e in (v'', (Set.union ae ae', Set.union ac ac'))) (v, (Set.empty, Set.empty)) ps
    menu' v (IntSel l r) e              = let (v', (ale, alc)) = menu' v l e
                                              (v'', (are, arc)) = menu' v' r e
                                          in (v'', (Set.union ale are, Set.union alc arc))                -- revisar
    menu' v (Seq l r) e                 = case l of
                                                Skip -> menu' v r e
                                                Stop -> (v, (Set.empty, Set.empty))
                                                p    -> menu' v l e
    menu' v (Inter l r) e               = menu' v l e                              -- revisar
                               --let (v', al) = menu' v l e
                                 --  (v'', ar) = menu' v' r e
                               --in (v'', Set.union al ar)
    
    chanMerge :: Set.Set Channel -> Set.Set Channel -> Set.Set Channel
    chanMerge x y = let xy = Set.union x y
                        (out, rest) = Set.partition (\c -> case c of
                                                                ComOut _ _ -> True
                                                                _          -> False) xy
                        chanMerge' o r = if Set.null o then r
                                                       else case Set.elemAt 0 o of
                                                                 ComOut n m -> if Set.member (ComIn n "") r then Set.insert (Com n m) $ chanMerge' (Set.deleteAt 0 o) (Set.delete (ComIn n "") r)
                                                                                                            else Set.insert (ComOut n m) $ chanMerge' (Set.deleteAt 0 o) r
                                                                 _          -> error "No deberia ocurrir, error en la combinacion de los canales"
                    in chanMerge' out rest
                                                                                                
                                                                                        
    getTrueEvents :: Set.Set Event -> Imp -> IO (Set.Set Event)
    getTrueEvents s i = do --print s
                           if Set.null s then return Set.empty
                                         else case Set.elemAt 0 s of
                                            Out n p -> do b <- execPred p i
                                                          if b then do s' <- getTrueEvents (Set.deleteAt 0 s) i
                                                                       return (Set.insert (Out n p) s')
                                                               else getTrueEvents (Set.deleteAt 0 s) i
                                            e       -> do s' <- getTrueEvents (Set.deleteAt 0 s) i
                                                          return (Set.insert e s')
                                            

    sustProc :: Value -> Id -> Env -> Proc -> Proc
    sustProc v x e p = fst $ sust' [] v x e p

    -- VER!!
--    noRep :: (Proc -> a) -> 
--    noRep = noRep' [] 
    
    sust' vis v x e Skip = (Skip, vis)
    sust' vis v x e Stop = (Stop, vis)
    sust' vis v x e (Prefix (C (ComOut n m)) p) = (Prefix (C (ComOut n ("let "++x++"="++v++" in "++m))) p, vis)
    sust' vis v x e (Prefix ev p) = (Prefix ev p, vis)
    sust' vis v x e (Ref s) = if elem s vis then (Ref s, vis)
                                            else case envLookup s e of
                                                      Just p -> sust' (s:vis) v x e p
                                                      Nothing -> error $ "Referencia a proceso "++s++" inexistente."
    sust' vis v x e (Parallel l r) = let (l', vis') = sust' vis v x e l
                                         (r', vis'') = sust' vis' v x e r
                                     in (Parallel l' r', vis'')
    sust' vis v x e (ExtSel ps) = let (ps', vvis) = foldl (\(p, vis') p' -> let (p'', vis'') = sust' vis' v x e p' in (p'':p, vis'')) ([], vis) ps
                                  in (ExtSel ps', vvis)
    sust' vis v x e (IntSel l r) = let (l', vis') = sust' vis v x e l
                                       (r', vis'') = sust' vis' v x e r
                                   in (IntSel l' r', vis'')
    sust' vis v x e (Seq l r)  = let (l', vis') = sust' vis v x e l
                                     (r', vis'') = sust' vis' v x e r
                                 in (Seq l' r', vis'')
    sust' vis v x e (Inter l r)  = let (l', vis') = sust' vis v x e l
                                       (r', vis'') = sust' vis' v x e r
                                   in (Inter l' r', vis'')

    eval :: Proc -> Event -> Env -> Imp -> IO Proc
    eval p e env i = case e of
                          C c -> evalChan p c env i
                          _   -> evalEvent p e env i
    
    evalEvent :: Proc -> Event -> Env -> Imp -> IO Proc
    evalEvent Skip _ _ _                        = return Skip
    evalEvent Stop _ _ _                        = return Stop
    evalEvent (Prefix e@(Out n pr) p) e' _ i    = if e==e' then return p 
                                                           else return Stop
    evalEvent (Prefix e@(In n a) p) e' _ i      = if e==e' then (execAct a i) >> return p
                                                           else return Stop
    evalEvent (Ref s) e en i                    = case envLookup s en of
                                                    Just p -> evalEvent p e en i                                       -- revisar problema de recursion
                                                    Nothing -> error $ "Referencia a proceso "++s++" inexistente" 
    evalEvent (Parallel l r) e en i             = case (Set.member e (menu l en), Set.member e (menu r en)) of
                                                    (True, True)   -> do l' <- evalEvent l e en i
                                                                         r' <- evalEvent r e en i
                                                                         return $ Parallel l' r'                                               -- sincronizacion
                                                    (True, False)  -> if Set.member e (alpha r en) then return Stop                            -- deadlock
                                                                                                   else do l' <- evalEvent l e en i
                                                                                                           return $ Parallel l' r              -- IEOF
                                                    (False, True)  -> if Set.member e (alpha l en) then return Stop                            -- deadlock
                                                                                                   else do r' <- evalEvent r e en i
                                                                                                           return $ Parallel l r'              -- IEOF
                                                    (False, False) -> return Stop                                                              -- error
    evalEvent (ExtSel ps) e en i                = maybe (return Stop) (\p -> evalEvent p e en i) (find (\p -> Set.member e (menu p en)) ps)
    evalEvent (IntSel l r) e en i               = evalEvent (ExtSel [l,r]) e en i                                                                  -- revisar!!!!!!!!!!!!
    evalEvent (Seq l r) e en i                  = case l of
                                                    Skip -> evalEvent r e en i
                                                    Stop -> return Stop
                                                    p    -> do l' <- evalEvent p e en i
                                                               return $ Seq l' r
    evalEvent (Inter l r) e en i                = if Set.member e (menu r en) then evalEvent r e en i
                                                                              else do l' <- evalEvent l e en i
                                                                                      return $ Inter l' r
                                                                                 
    evalChan :: Proc -> Channel -> Env -> Imp -> IO Proc
    evalChan Skip _ _ _                        = return Skip
    evalChan Stop _ _ _                        = return Stop
    evalChan (Prefix (C (ComOut n _)) p) c _ _ = if nameOfCom c == n then return p                          -- deberia chequear que no se llamen igual canales y eventos
                                                                     else return Stop
    evalChan (Prefix (C (ComIn n var)) p) (Com c exp) env i = if c == n then do --val <- execExp exp i
                                                                                return $ sustProc exp var env p
                                                                        else return Stop
    evalChan (Prefix e p) _ _ _                  = return Stop
    evalChan (Ref s) c env i                    = case envLookup s env of
                                                    Just p -> evalChan p c env i                                       -- revisar problema de recursion
                                                    Nothing -> error $ "Referencia a proceso "++s++" inexistente" 
    evalChan (Parallel l r) c@(Com n e) env i | Set.member c ml                                          = do l' <- evalChan l c env i
                                                                                                              return $ Parallel l' r
                                              | Set.member c mr                                          = do r' <- evalChan r c env i
                                                                                                              return $ Parallel l r'
                                              | (Set.member (ComIn n "") ml && Set.member (ComOut n e) mr) ||
                                                (Set.member (ComIn n "") mr && Set.member (ComOut n e) ml) = do l' <- evalChan l c env i
                                                                                                                r' <- evalChan r c env i
                                                                                                                return $ Parallel l' r'
                                              | otherwise                                                  = return Stop
                                                    where ml = menuCom l env
                                                          mr = menuCom r env
    evalChan (ExtSel ps) c@(Com n e) env i      = maybe (return Stop) (\p -> evalChan p c env i) (find (\p -> let m = menuCom p env 
                                                                                                              in Set.member c m || Set.member (ComIn n "") m || Set.member (ComOut n e) m) ps)
    evalChan (IntSel l r) c env i               = evalChan (ExtSel [l,r]) c env i                                                                  -- revisar!!!!!!!!!!!!
    evalChan (Seq l r) c env i                   = case l of
                                                Skip -> evalChan r c env i
                                                Stop -> return Stop
                                                p    -> do l' <- evalChan p c env i
                                                           return $ Seq l' r
    evalChan (Inter l r) c env i                = if Set.member c (menuCom r env) then evalChan r c env i
                                                                                  else do l' <- evalChan l c env i
                                                                                          return $ Inter l' r
 
    
    
    -- ASUMO QUE LOS PREDICADOS Y ACCIONES SON PARA CADA APARICION DEL EVENTO EN EL PROCESO
    setPredAct :: [ProcDef] -> [Claus] -> [ProcDef]
    setPredAct defs [] = defs
    setPredAct defs ((CPred e p pr):cs) = case find (\(Def name proc) -> name == p) defs of
                                               Just (Def name proc) -> setPredAct ((Def name (setPredProc e pr proc)):(delete (Def name proc) defs)) cs
                                               Nothing              -> error $ "Clausulas: referencia a proceso "++p++" inexistente."
    setPredAct defs ((CAct e p a):cs) = case find (\(Def name proc) -> name == p) defs of
                                               Just (Def name proc) -> setPredAct ((Def name (setActProc e a proc)):(delete (Def name proc) defs)) cs
                                               Nothing              -> error $ "Clausulas: referencia a proceso "++p++" inexistente."
    
    
    setPredProc e pr Skip = Skip
    setPredProc e pr Stop = Stop
    setPredProc e pr (Prefix (Out e' pr') p) = if e==e' then Prefix (Out e pr) p
                                                        else Prefix (Out e' pr') (setPredProc e pr p)
    setPredProc e pr (Prefix (In e' a) p) = if e==e' then error $ "Clausulas: predicado para evento "++e++" de entrada."          -- revisar
                                                     else Prefix (In e' a) (setPredProc e pr p)
    setPredProc e pr (Parallel l r) = Parallel (setPredProc e pr l) (setPredProc e pr r)
    setPredProc e pr (ExtSel ps) = ExtSel $ map (setPredProc e pr) ps
    setPredProc e pr (IntSel l r) = IntSel (setPredProc e pr l) (setPredProc e pr r)
    setPredProc e pr (Seq l r) = Seq (setPredProc e pr l) (setPredProc e pr r)
    setPredProc e pr (Inter l r) = Inter (setPredProc e pr l) (setPredProc e pr r)

    setActProc e a Skip = Skip
    setActProc e a Stop = Stop
    setActProc e a (Prefix (In e' a') p) = if e==e' then Prefix (In e a) p
                                                    else Prefix (In e' a') (setActProc e a p)
    setActProc e a (Prefix (Out e' pr') p) = if e==e' then error $ "Clausulas: accion para evento "++e++" de salida."          -- revisar
                                                      else Prefix (Out e' pr') (setActProc e a p)
    setActProc e a (Parallel l r) = Parallel (setActProc e a l) (setActProc e a r)
    setActProc e a (ExtSel ps) = ExtSel $ map (setActProc e a) ps
    setActProc e a (IntSel l r) = IntSel (setActProc e a l) (setActProc e a r)
    setActProc e a (Seq l r) = Seq (setActProc e a l) (setActProc e a r)
    setActProc e a (Inter l r) = Inter (setActProc e a l) (setActProc e a r)    
  
  
                          
    
----------------------------
--- Print
----------------------------
    

    printEvent (Out s p) = "_out_ "++s++" "
    printEvent (In s f) = "_in_ "++s++" "
    printEvent (C (ComOut n m)) = "_chan_ "++n++"!"++m++" "
    printEvent (C (ComIn n m)) = "_chan_ "++n++"?"++m++" "
    printEvent (C (Com n m)) = "_chan_ "++n++"."++m++" "
    
    strProc Skip = "skip;"
    strProc Stop = "stop;"
    strProc (Ref s) = s
    strProc (Prefix e p) = printEvent e ++ " -> " ++ strProc p
    strProc (Parallel l r) = strProc l ++ " || " ++ strProc r
    strProc (ExtSel ps) = foldl (\s p -> s ++ " [] " ++ strProc p) "" ps
    strProc (IntSel l r) = strProc l ++ " /| " ++ strProc r
    strProc (Seq l r) = strProc l ++ " ; " ++ strProc r
    strProc (Inter l r) = strProc l ++ "|>" ++ strProc r

    printProc :: Proc -> IO ()
    printProc p = putStrLn $ strProc p
    
    strDefs :: [ProcDef] -> [String]
    strDefs ds = map strdef ds
        where strdef (Def name p) = name ++ " = " ++ (strProc p)
    
    printDefs :: [ProcDef] -> IO ()
    printDefs ds = mapM_ putStrLn (strDefs ds)
