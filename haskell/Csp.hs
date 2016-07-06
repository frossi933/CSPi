{-# LANGUAGE ExistentialQuantification #-}
module Csp where

    import Data.List
    import Env

    import qualified EventSet as Set    
--    import qualified Data.Set as Set
    import Common
    import Exec
    import Control.Monad
    import Control.Concurrent
    import System.Random

  

    chkNames :: [ProcDef] -> IO Bool
    chkNames [] =  return True
    chkNames ((Def name e p):ds) = case find (\(Def name' e' p') -> name == name') ds of
                                            Just (Def n _ _) -> do putStrLn ("Error: multiples definiciones de "++n++".")
                                                                   return False
                                            Nothing          -> chkNames ds
    
      
    sistema :: ProcEnv -> IO Proc
    sistema e = return $ maybe (maybe Stop id (envGetRef "Sistema" [] e)) id (envGetRef "SISTEMA" [] e)                     
{-                    
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

  -}  
    menu :: Proc -> IO EvSet
    menu Stop = return Set.empty
    menu Skip = return Set.empty
    menu (Prefix v@(E _ Nothing _) _) = return $ Set.singleton v
    menu (Prefix v@(E id (Just bvar) _) _) = do 
                                                b <- takeMVar bvar
                                                when debug $ print ("Checking mvar state of "++id++(show b))
                                                --threadDelay 1000000
                                                --putMVar bvar b
                                                --when debug $ print ("Mvar checked")
                                                if b then (return $ Set.singleton v) else return Set.empty
    menu (ExtSel Skip p) = do mp <- menu p
                              return $ Set.insert Eps mp
    menu (ExtSel p Skip) = do mp <- menu p
                              return $ Set.insert Eps mp
    menu (ExtSel Stop Stop) = return $ Set.singleton Eps
    menu (ExtSel p q) = do mp <- menu p
                           mq <- menu q
                           return $ Set.union mp mq
    menu (IntSel p q) = return $ Set.singleton Eps
    menu (Seq Skip q) = return $ Set.singleton Eps
    menu (Seq Stop q) = return $ Set.singleton Eps
    menu (Seq p q) = menu p
    menu (Inter p q) = do mp <- menu p
                          mq <- menu q
                          return $ Set.union mp mq
    menu (Parallel _ Skip Skip) = return $ Set.singleton Eps
    menu (Parallel s p q) = do mp <- menu p
                               mq <- menu q
                               return (Set.union (Set.intersection (Set.intersection mp mq) s)
                                                 (Set.union (Set.difference mp s) (Set.difference mq s)))
    menu (Ref s e) = return $ Set.singleton Eps


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
    smallstep_eval :: ProcEnv -> Proc -> Event -> Maybe Proc
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
    smallstep_eval e (Ref name exp) Eps = envGetRef name exp e -- TODO:add exp
    smallstep_eval e (Ref name exp) _ = Nothing
    smallstep_eval e (Inter p q) v = case smallstep_eval e q v of
                                        Just q1 -> Just q1
                                        Nothing -> case smallstep_eval e p v of
                                                    Just p1 -> Just (Inter p1 q)
                                                    Nothing -> Nothing -- ??

    choose :: EvSet -> IO Event
    choose s = do when debug $ putStrLn (show s)
                  if Set.member Eps s then return Eps else (do r <- randomRIO (0, (Set.size s)-1)       -- Eps has higher priority than concrete events
                                                               return (Set.elemAt r s))


    eval :: ProcEnv -> Imp -> Proc -> IO Proc
    eval e _ Stop = do putStrLn "END: Stop"
                       return Stop
    eval e _ Skip = do putStrLn "END: Success"
                       return Skip
    eval e i p = do m <- menu p
                    if Set.null m then eval e i p                   -- waits for available events 
                                  else do v <- choose m
                                          when debug $ putStrLn ("Menu: "++ show m ++ " Evento: " ++ show v ++ " -- Proceso: " ++ show p)
                                          case v of
                                            E _ Nothing Nothing -> case smallstep_eval e p v of
                                                                       Nothing -> do putStrLn "END: Internal error"
                                                                                     return Stop
                                                                       Just p1 -> eval e i p1
                                            E id (Just bvar) Nothing -> case smallstep_eval e p v of
                                                                Nothing -> do putStrLn "END: Internal error"
                                                                              return Stop
                                                                Just p1 -> do swapMVar bvar initial_state_mvar -- It could lose changes in bvar between its check in menu and this point
                                                                              eval e i p1
                                            E id Nothing (Just act) -> case smallstep_eval e p v of
                                                                        Nothing -> do putStrLn "END: Internal error"
                                                                                      return Stop
                                                                        Just p1 -> do execAct act i
                                                                                      eval e i p1
                                            E id (Just bvar) (Just act) -> case smallstep_eval e p v of
                                                                            Nothing -> do putStrLn "END: Internal error"
                                                                                          return Stop
                                                                            Just p1 -> do swapMVar bvar initial_state_mvar
                                                                                          execAct act i
                                                                                          eval e i p1
                                            _ -> case smallstep_eval e p v of
                                                    Nothing -> do putStrLn "END: Internal error"
                                                                  return Stop
                                                    Just p1 -> eval e i p1    

    

    setActAndVars :: [ProcDef] -> [Claus] -> PredMap -> [ProcDef]
    setActAndVars defs [] _ = defs
    setActAndVars defs ((CPred id _ pred):cs) vars = setActAndVars (map (\(Def name exp proc) -> Def name exp (setVarProc id vars proc)) defs) cs vars
    setActAndVars defs ((CAct id _ act):cs) vars = setActAndVars (map (\(Def name exp proc) -> Def name exp (setActProc id act proc)) defs) cs vars
    
    setVarProc :: String -> PredMap -> Proc -> Proc
    setVarProc _ _ Skip = Skip
    setVarProc _ _ Stop = Stop
    setVarProc _ _ (Ref name exp) = Ref name exp
    setVarProc id vars (Prefix e@(E id' Nothing act) p) = if id==id' then case envGetVar id vars of
                                                                Just mvar -> Prefix (E id (Just mvar) act) (setVarProc id vars p)
                                                                Nothing -> error $ "No existe MVar correspondiente a "++id
                                                                     else Prefix e (setVarProc id vars p)
    setVarProc id vars (Prefix e@(E id' (Just mvar) act) p) = if id==id' then error $ "Ya existe MVar correspondiente a "++id
                                                                         else Prefix e (setVarProc id vars p)
    setVarProc id vars (Prefix e p) = Prefix e (setVarProc id vars p)
    setVarProc id pr (Parallel s l r) = Parallel s (setVarProc id pr l) (setVarProc id pr r)
    setVarProc id pr (ExtSel p q) = ExtSel (setVarProc id pr p) (setVarProc id pr q) 
    setVarProc id pr (IntSel l r) = IntSel (setVarProc id pr l) (setVarProc id pr r)
    setVarProc id pr (Seq l r) = Seq (setVarProc id pr l) (setVarProc id pr r)
    setVarProc id pr (Inter l r) = Inter (setVarProc id pr l) (setVarProc id pr r)

    setActProc :: String -> Act -> Proc -> Proc
    setActProc _ _ Skip = Skip
    setActProc _ _ Stop = Stop
    setActProc _ _ (Ref name exp) = Ref name exp
    setActProc id act (Prefix e@(E id' v Nothing) p) = if id==id' then Prefix (E id v (Just act)) (setActProc id act p)
                                                                  else Prefix e (setActProc id act p)
    setActProc id act (Prefix e@(E id' v (Just act')) p) = if id==id' then error $ "Ya existe accion para evento "++id
                                                                      else Prefix e (setActProc id act p)
    setActProc id act (Prefix e p) = Prefix e (setActProc id act p)
    setActProc id a (Parallel s l r) = Parallel s (setActProc id a l) (setActProc id a r)
    setActProc id a (ExtSel p q) = ExtSel (setActProc id a p) (setActProc id a q)
    setActProc id a (IntSel l r) = IntSel (setActProc id a l) (setActProc id a r)
    setActProc id a (Seq l r) = Seq (setActProc id a l) (setActProc id a r)
    setActProc id a (Inter l r) = Inter (setActProc id a l) (setActProc id a r)    
  
  
                          
    
----------------------------
--- Print
----------------------------
    

    printEvent (E id v a) = "_event_("++id++","++(show v)++","++(show a)++")"                           ------
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
