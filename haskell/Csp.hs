{-# LANGUAGE ExistentialQuantification #-}
module Csp where

    import Data.List
    import Env

    import qualified EventSet as Set    
--    import qualified Data.Set as Set
    import Common
    import Exec
--    import Control.Monad.Random                
    import System.Random

----------------- DEBUG FLAG
    debug :: Bool
    debug = True

   

    chkNames :: [ProcDef] -> IO Bool
    chkNames [] =  return True
    chkNames ((Def name e p):ds) = case find (\(Def name' e' p') -> name == name') ds of
                                            Just (Def n _ _) -> do putStrLn ("Error: multiples definiciones de "++n++".")
                                                                   return False
                                            Nothing          -> chkNames ds
    
      
    sistema :: Env -> IO Proc
    sistema e = return $ maybe (maybe Stop id (envLookup "Sistema" e)) id (envLookup "SISTEMA" e)                     
                    
    alpha :: Proc -> Env -> EvSet
    alpha p e = snd $ alpha' ["Sistema","SISTEMA"] p e
    
    alpha' :: [String] -> Proc -> Env -> ([String], EvSet)
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

    
    menu :: Imp -> Proc -> IO EvSet
    menu _ Stop = return Set.empty
    menu _ Skip = return Set.empty
    menu i (Prefix v@(In id "") p) = return $ Set.singleton v
    menu i (Prefix v@(In id pred) p) = do b <- execPred pred i
                                          if b then return $ Set.singleton v 
                                               else return Set.empty
    menu _ (Prefix v p) = return $ Set.singleton v
    menu i (ExtSel Skip p) = do mp <- menu i p
                                return $ Set.insert Eps mp
    menu i (ExtSel p Skip) = do mp <- menu i p
                                return $ Set.insert Eps mp
    menu _ (ExtSel Stop Stop) = return $ Set.singleton Eps
    menu i (ExtSel p q) = do mp <- menu i p
                             mq <- menu i q
                             return $ Set.union mp mq
    menu _ (IntSel p q) = return $ Set.singleton Eps
    menu _ (Seq Skip q) = return $ Set.singleton Eps
    menu _ (Seq Stop q) = return $ Set.singleton Eps
    menu i (Seq p q) = menu i p
    menu i (Inter p q) = do mp <- menu i p
                            mq <- menu i q
                            return $ Set.union mp mq
    menu _ (Parallel _ Skip Skip) = return $ Set.singleton Eps
    menu i (Parallel s p q) = do mp <- menu i p
                                 mq <- menu i q
                                 return (Set.union (Set.intersection (Set.intersection mp mq) s)
                                                   (Set.union (Set.difference mp s) (Set.difference mq s)))
    menu _ (Ref s e) = return $ Set.singleton Eps


    {-
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

    
-}
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
    smallstep_eval _ (IntSel p q) _   = Nothing
    smallstep_eval _ (Seq Skip p) Eps = Just p
    smallstep_eval _ (Seq Stop _) Eps = Just Stop
    smallstep_eval e (Seq p q) v = case smallstep_eval e p v of
                                    Just p1 -> Just (Seq p1 q)
                                    Nothing -> Nothing
    smallstep_eval _ (Parallel _ Skip Skip) Eps = Just Skip
    smallstep_eval e (Parallel s p q) Eps = case (smallstep_eval e p Eps, smallstep_eval e q Eps) of
                                                (Just p1, Nothing) -> Just (Parallel s p1 q)
                                                (Nothing, Just q1) -> Just (Parallel s p q1)
                                                (Just p1, Just q1) -> Just (Parallel s p1 q) -- TODO: random selection
                                                _                  -> Nothing 
    smallstep_eval e (Parallel s p q) v = if Set.member v s then case (smallstep_eval e p v, smallstep_eval e q v) of
                                                                    (Just p1, Just q1) -> Just (Parallel s p1 q1)
                                                                    _                  -> Nothing
                                                            else case (smallstep_eval e p v, smallstep_eval e q v) of
                                                                    (Just p1, Nothing) -> Just (Parallel s p1 q)
                                                                    (Nothing, Just q1) -> Just (Parallel s p q1)
                                                                    _                  -> Nothing
    smallstep_eval e (Ref name exp) Eps = envLookup name e -- TODO:add exp
    smallstep_eval e (Ref name exp) _ = Nothing
    smallstep_eval e (Inter p q) v = case smallstep_eval e q v of
                                        Just q1 -> Just q1
                                        Nothing -> case smallstep_eval e p v of
                                                    Just p1 -> Just (Inter p1 q)
                                                    Nothing -> Nothing -- ??

    choose :: EvSet -> IO Event
    choose s = do if debug then putStrLn (show s) else print ""
                  if Set.member Eps s then return Eps else (do r <- randomRIO (0, (Set.size s)-1)       -- Eps has higher priority than concrete events
                                                               return (Set.elemAt r s))


    eval :: Env -> Imp -> Proc -> IO Proc
    eval e _ Stop = do putStrLn "END: Stop"
                       return Stop
    eval e _ Skip = do putStrLn "END: Success"
                       return Skip
    eval e i p = do m <- menu i p
                    if Set.null m then do putStrLn "END: No progress"
                                          return Stop -- TODO: wait for available events 
                                  else do v <- choose m
                                          if debug then putStrLn ("Evento: " ++ show v ++ " -- Proceso: " ++ show p) else print ""
                                          case v of
                                            Out id "" -> case smallstep_eval e p v of
                                                                Nothing -> do putStrLn "END: Internal error"
                                                                              return Stop
                                                                Just p1 -> eval e i p1
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
                                               Nothing                  -> error $ "Clausulas: referencia a proceso "++p++" inexistente."
    setPredAct defs ((CAct e p a):cs) = case find (\(Def name exp proc) -> name == p) defs of
                                               Just (Def name exp proc) -> setPredAct ((Def name exp (setActProc e a proc)):(delete (Def name exp proc) defs)) cs
                                               Nothing                  -> error $ "Clausulas: referencia a proceso "++p++" inexistente."
    
    
    setPredProc _ _ Skip = Skip
    setPredProc _ _ Stop = Stop
    setPredProc id pr (Prefix (In id' pr') p) = if id==id' then Prefix (In id pr) p
                                                           else Prefix (In id' pr') (setPredProc id pr p)
    setPredProc id pr (Prefix (Out id' a) p) = if id==id' then error $ "Clausulas: predicado para evento "++id++" de salida."          -- revisar
                                                          else Prefix (Out id' a) (setPredProc id pr p)
    setPredProc id pr (Parallel s l r) = Parallel s (setPredProc id pr l) (setPredProc id pr r)
    setPredProc id pr (ExtSel p q) = ExtSel (setPredProc id pr p) (setPredProc id pr q) 
    setPredProc id pr (IntSel l r) = IntSel (setPredProc id pr l) (setPredProc id pr r)
    setPredProc id pr (Seq l r) = Seq (setPredProc id pr l) (setPredProc id pr r)
    setPredProc id pr (Inter l r) = Inter (setPredProc id pr l) (setPredProc id pr r)

    setActProc _ _ Skip = Skip
    setActProc _ _ Stop = Stop
    setActProc id a (Prefix (Out id' a') p) = if id==id' then Prefix (Out id a) p
                                                         else Prefix (Out id' a') (setActProc id a p)
    setActProc id a (Prefix (In id' pr') p) = if id==id' then error $ "Clausulas: accion para evento "++id++" de salida."          -- revisar
                                                         else Prefix (In id' pr') (setActProc id a p)
    setActProc id a (Parallel s l r) = Parallel s (setActProc id a l) (setActProc id a r)
    setActProc id a (ExtSel p q) = ExtSel (setActProc id a p) (setActProc id a q)
    setActProc id a (IntSel l r) = IntSel (setActProc id a l) (setActProc id a r)
    setActProc id a (Seq l r) = Seq (setActProc id a l) (setActProc id a r)
    setActProc id a (Inter l r) = Inter (setActProc id a l) (setActProc id a r)    
  
  
                          
    
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
