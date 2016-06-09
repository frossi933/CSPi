{-# LANGUAGE ExistentialQuantification #-}
module Csp where

    import Data.List
    import Env
    import qualified Data.Set as Set
    import Common
    import Exec
--    import Control.Monad.Random                
    import System.Random

   

    chkNames :: [ProcDef] -> IO Bool
    chkNames [] =  return True
    chkNames ((Def name e p):ds) = case find (\(Def name' e' p') -> name == name') ds of
                                            Just (Def n _ _) -> do putStrLn ("Error: multiples definiciones de "++n++".")
                                                                   return False
                                            Nothing          -> chkNames ds
    
      
    sistema :: Env -> IO Proc
    sistema e = return $ maybe (maybe Stop id (envLookup "Sistema" e)) id (envLookup "SISTEMA" e)                     
                    
    alpha :: Proc -> Env -> Set.Set Event
    alpha p e = snd $ alpha' ["Sistema","SISTEMA"] p e
    
    alpha' :: [String] -> Proc -> Env -> ([String], Set.Set Event)
    alpha' v Skip _ = (v, Set.empty) -- revisar
    alpha' v Stop _ = (v, Set.empty)
    alpha' v (Ref s ex) e = if elem s v then (v, Set.empty)
                                     else case envLookup s e of
                                            Just p -> alpha' (s:v) p e
                                            Nothing -> error $ "Referencia a proceso "++s++" inexistente"
    alpha' v (Prefix ev p) e = let (v', a) = alpha' v p e in (v', Set.insert ev a)
    alpha' v (Parallel _ l r) e = let (v', al) = alpha' v l e
                                      (v'', ar) = alpha' v' r e
                                  in (v'', Set.union al ar)
    alpha' v (ExtSel p q) e = foldl (\(v',a) p -> let (v'', a') = alpha' v' p e in (v'', Set.union a a')) (v, Set.empty) [p,q]
    alpha' v (IntSel l r) e = let (v', al) = alpha' v l e
                                  (v'', ar) = alpha' v' r e
                              in (v'', Set.union al ar)
    alpha' v (Seq l r) e = let (v', al) = alpha' v l e
                               (v'', ar) = alpha' v' r e
                           in (v'', Set.union al ar)
    alpha' v (Inter l r) e = let (v', al) = alpha' v l e
                                 (v'', ar) = alpha' v' r e
                             in (v'', Set.union al ar)

    
    menu :: Proc -> EvSet
    menu Stop = Set.empty
    menu Skip = Set.empty
    menu (Prefix v@(In id pred) p) = if True then Set.singleton v else Set.empty
    menu (Prefix v p) = Set.singleton v
    menu (ExtSel Skip p) = Set.insert Eps (menu p)
    menu (ExtSel p Skip) = Set.insert Eps (menu p)
    menu (ExtSel Stop Stop) = Set.singleton Eps
    menu (ExtSel p q) = Set.union (menu p) (menu q)
    menu (IntSel p q) = Set.singleton Eps
    menu (Seq Skip q) = Set.singleton Eps
    menu (Seq Stop q) = Set.singleton Eps
    menu (Seq p q) = menu p
    menu (Inter p q) = Set.union (menu q) (menu p)
    menu (Parallel _ Skip Skip) = Set.singleton Eps
    menu (Parallel s p q) = let mp = menu p
                                mq = menu q
                            in Set.union (Set.intersection (Set.intersection mp mq) s)
                                         (Set.union (Set.difference mp s) (Set.difference mq s))
    menu (Ref s e) = Set.singleton Eps
{-
    menu :: Proc -> Env -> EvSet
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
    menu' v (Ref s exp) e               = if elem s v then (v, (Set.empty, Set.empty))                      -- ya lo visitamos
                                                      else case envGetRef s exp e of
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
-}    
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
    sust' vis v x e (Ref s ex) = if elem s vis then (Ref s ex, vis)
                                               else case envLookup s e of
                                                      Just p -> sust' (s:vis) v x e p
                                                      Nothing -> error $ "Referencia a proceso "++s++" inexistente."
    sust' vis v x e (Parallel s l r) = let (l', vis') = sust' vis v x e l
                                           (r', vis'') = sust' vis' v x e r
                                       in (Parallel s l' r', vis'')
    sust' vis v x e (ExtSel p q) = let (ps'@[p',q'], vvis) = foldl (\(p, vis') p' -> let (p'', vis'') = sust' vis' v x e p' in (p'':p, vis'')) ([], vis) [p, q]
                                  in (ExtSel p' q', vvis)
    sust' vis v x e (IntSel l r) = let (l', vis') = sust' vis v x e l
                                       (r', vis'') = sust' vis' v x e r
                                   in (IntSel l' r', vis'')
    sust' vis v x e (Seq l r)  = let (l', vis') = sust' vis v x e l
                                     (r', vis'') = sust' vis' v x e r
                                 in (Seq l' r', vis'')
    sust' vis v x e (Inter l r)  = let (l', vis') = sust' vis v x e l
                                       (r', vis'') = sust' vis' v x e r
                                   in (Inter l' r', vis'')

    

    smallstep_eval :: Env -> Proc -> Event -> Maybe Proc
    smallstep_eval _ Stop _ = Nothing
    smallstep_eval _ Skip _ = Nothing
    smallstep_eval _ (Prefix e p) v = if (e == v) then Just p
                                                else Nothing
    smallstep_eval _ (ExtSel Stop Stop) Eps = Just Stop
    smallstep_eval _ (ExtSel Skip p) Eps = Just p
    smallstep_eval _ (ExtSel p Skip) Eps = Just p
    smallstep_eval e (ExtSel p q) Eps = case (smallstep_eval e p Eps, smallstep_eval e q Eps) of
                                        (Nothing, Nothing) -> Nothing
                                        (Just p1, Nothing) -> Just (ExtSel p1 q)
                                        (Nothing, Just q1) -> Just (ExtSel p q1)
                                        (Just p1, Just q1) -> Just (ExtSel p1 q) -- TODO: random selection
    smallstep_eval e (ExtSel p q) v = case (smallstep_eval e p v, smallstep_eval e q v) of
                                        (Nothing, Nothing) -> Nothing
                                        (Just p1, Nothing) -> Just p1
                                        (Nothing, Just q1) -> Just q1
                                        (Just p1, Just q1) -> Just p1 -- TODO: random selection
    smallstep_eval _ (IntSel p q) Eps = Just p -- TODO: random selection
    smallstep_eval _ (Seq Skip p) Eps = Just p
    smallstep_eval _ (Seq Stop _) Eps = Just Stop
    smallstep_eval e (Seq p q) v = case smallstep_eval e p v of
                                    Just p1 -> Just (Seq p1 q)
                                    Nothing -> Nothing
    smallstep_eval _ (Parallel _ Skip Skip) Eps = Just Skip 
    smallstep_eval e (Parallel s p q) v = if Set.member v s then case (smallstep_eval e p v, smallstep_eval e q v) of
                                                                    (Just p1, Just q1) -> Just (Parallel s p1 q1)
                                                                    _                  -> Nothing
                                                          else case (smallstep_eval e p v, smallstep_eval e q v) of
                                                                    (Just p1, Nothing) -> Just (Parallel s p1 q)
                                                                    (Nothing, Just q1) -> Just (Parallel s p q1)
                                                                    _                  -> Nothing
    smallstep_eval e (Ref name exp) Eps = envLookup name e -- TODO:add exp
    smallstep_eval e (Inter p q) v = case smallstep_eval e q v of
                                        Just q1 -> Just q1
                                        Nothing -> case smallstep_eval e p v of
                                                    Just p1 -> Just (Inter p1 q)
                                                    Nothing -> Nothing -- ??

    choose :: EvSet -> IO Event
    choose s = do r <- randomRIO (0, (Set.size s)-1)
                  return (Set.elemAt r s)


    eval :: Env -> Imp -> Proc -> IO Proc
    eval e _ Stop = do putStrLn "END: Stop"
                       return Stop
    eval e _ Skip = do putStrLn "END: Success"
                       return Skip
    eval e i p = let m = menu p
                 in if Set.null m then do putStrLn "END: No progress"
                                          return Stop -- TODO: wait for available events 
                                  else do v <- choose m
                                          case v of
                                            Out id act -> case smallstep_eval e p v of
                                                                Nothing -> do putStrLn "END: Internal error"
                                                                              return Stop
                                                                Just p1 -> do execAct act i
                                                                              eval e i p1
                                            _ -> case smallstep_eval e p v of
                                                    Nothing -> do putStrLn "END: Internal error"
                                                                  return Stop
                                                    Just p1 -> eval e i p1
                    
 
 
    
    
    -- ASUMO QUE LOS PREDICADOS Y ACCIONES SON PARA CADA APARICION DEL EVENTO EN EL PROCESO
    setPredAct :: [ProcDef] -> [Claus] -> [ProcDef]
    setPredAct defs [] = defs
    setPredAct defs ((CPred e p pr):cs) = case find (\(Def name exp proc) -> name == p) defs of
                                               Just (Def name exp proc) -> setPredAct ((Def name exp (setPredProc e pr proc)):(delete (Def name exp proc) defs)) cs
                                               Nothing              -> error $ "Clausulas: referencia a proceso "++p++" inexistente."
    setPredAct defs ((CAct e p a):cs) = case find (\(Def name exp proc) -> name == p) defs of
                                               Just (Def name exp proc) -> setPredAct ((Def name exp (setActProc e a proc)):(delete (Def name exp proc) defs)) cs
                                               Nothing              -> error $ "Clausulas: referencia a proceso "++p++" inexistente."
    
    
    setPredProc e pr Skip = Skip
    setPredProc e pr Stop = Stop
    setPredProc e pr (Prefix (Out e' pr') p) = if e==e' then Prefix (Out e pr) p
                                                        else Prefix (Out e' pr') (setPredProc e pr p)
    setPredProc e pr (Prefix (In e' a) p) = if e==e' then error $ "Clausulas: predicado para evento "++e++" de entrada."          -- revisar
                                                     else Prefix (In e' a) (setPredProc e pr p)
    setPredProc e pr (Parallel s l r) = Parallel s (setPredProc e pr l) (setPredProc e pr r)
    setPredProc e pr (ExtSel p q) = ExtSel (setPredProc e pr p) (setPredProc e pr q) 
    setPredProc e pr (IntSel l r) = IntSel (setPredProc e pr l) (setPredProc e pr r)
    setPredProc e pr (Seq l r) = Seq (setPredProc e pr l) (setPredProc e pr r)
    setPredProc e pr (Inter l r) = Inter (setPredProc e pr l) (setPredProc e pr r)

    setActProc e a Skip = Skip
    setActProc e a Stop = Stop
    setActProc e a (Prefix (In e' a') p) = if e==e' then Prefix (In e a) p
                                                    else Prefix (In e' a') (setActProc e a p)
    setActProc e a (Prefix (Out e' pr') p) = if e==e' then error $ "Clausulas: accion para evento "++e++" de salida."          -- revisar
                                                      else Prefix (Out e' pr') (setActProc e a p)
    setActProc e a (Parallel s l r) = Parallel s (setActProc e a l) (setActProc e a r)
    setActProc e a (ExtSel p q) = ExtSel (setActProc e a p) (setActProc e a q)
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
    strProc (Ref s e) = s
    strProc (Prefix e p) = printEvent e ++ " -> " ++ strProc p
    strProc (Parallel s l r) = strProc l ++ " || " ++ strProc r
    strProc (ExtSel p q) = strProc q ++ " [] " ++ strProc q
    strProc (IntSel l r) = strProc l ++ " /| " ++ strProc r
    strProc (Seq l r) = strProc l ++ " ; " ++ strProc r
    strProc (Inter l r) = strProc l ++ "|>" ++ strProc r

    printProc :: Proc -> IO ()
    printProc p = putStrLn $ strProc p
    
    strDefs :: [ProcDef] -> [String]
    strDefs ds = map strdef ds
        where strdef (Def name exp p) = name ++ " = " ++ (strProc p)
    
    printDefs :: [ProcDef] -> IO ()
    printDefs ds = mapM_ putStrLn (strDefs ds)
